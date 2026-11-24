(in-package #:harness-demos-browser)

;;;; browser-protocol 0.2.0 against a real headless Chrome: the Playwright /
;;;; Selenium jobs from the python-to-cl page (launch, goto that waits for the
;;;; page, evaluate, screenshot, PDF), and the three fixes behind it:
;;;; browser-protocol#1 (stack JSON and base64), #2 (launch, replies by id,
;;;; navigate waits) and #3 (*browser-backend* for the NIL-backend methods).
;;;; Every result with a known answer is checked: RUN-DEMOS returns NIL when one
;;;; differs or a section stops on an error.

(defvar *checks* 0)
(defvar *mismatches* 0)
(defvar *failed-sections* '())

(defun shown (text)
  "TEXT with the home and temporary directories replaced by ~ and <tmp>."
  (let ((text (princ-to-string text)))
    (flet ((swap (from to)
             (let ((from (string-right-trim "/" from)))
               (loop for at = (search from text)
                     while (and at (plusp (length from)))
                     do (setf text (concatenate 'string (subseq text 0 at) to
                                                (subseq text (+ at (length from)))))))))
      (swap (namestring (uiop:temporary-directory)) "<tmp>")
      (swap (namestring (user-homedir-pathname)) "~")
      text)))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  (format t "~&~{~a~%~}~%" lines))

(defun show (label value)
  (format t "~&  ~38a ~a~%" label (shown value)))

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (let ((ok (funcall test got expected)))
    (unless ok (incf *mismatches*))
    (format t "~&  ~:[MISMATCH~;ok      ~] ~a: ~a~:[ (want ~a)~;~*~]~%"
            ok label (shown (prin1-to-string got)) ok (prin1-to-string expected))
    ok))

(defmacro section (title &body body)
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (format t "~&  SECTION FAILED: ~a: ~a~%" (type-of c) (shown c))))))

(defun seconds-since (start)
  (/ (round (* 10 (/ (- (get-internal-real-time) start) internal-time-units-per-second))) 10.0))

(defparameter *chrome-navigate-reply*
  "{\"id\":1,\"result\":{\"frameId\":\"13AAB0BE9ACCA194BE5D8B66BD278795\",\"loaderId\":\"B5DEC2C3688A1476E494B6B15132FBA5\",\"isDownload\":false}}"
  "Chrome's Page.navigate reply, verbatim (browser-protocol#1).")

(defun png-size (octets)
  "Width and height from a PNG's IHDR chunk."
  (flet ((u32 (at) (reduce (lambda (acc b) (+ (* acc 256) b)) (subseq octets at (+ at 4)))))
    (list (u32 16) (u32 20))))

(defun demo-chrome-json ()
  (section "1. Chrome's JSON and base64 go through the stack (#1)"
    (para "0.1.0 parsed CDP JSON with a private parser whenever no json-protocol backend"
          "was bound, and that parser could not read `false`: Chrome's Page.navigate reply"
          "(\"isDownload\":false) failed with \"bad JSON false\". 0.2.0 depends on"
          "json-protocol + json-backend-jzon and encoding-protocol; the private parser and"
          "base64 are gone.")
    (let* ((msg (b:decode-cdp-message *chrome-navigate-reply*))
           (result (b:cdp-message-result msg)))
      (show "decode-cdp-message →" (type-of msg))
      (check "frameId" (gethash "frameId" result) "13AAB0BE9ACCA194BE5D8B66BD278795")
      (multiple-value-bind (value present) (gethash "isDownload" result)
        (check "isDownload (false → NIL, present)" (list value present) '(nil t))))
    (check "null → :NULL"
           (gethash "x" (b:cdp-message-result (b:decode-cdp-message "{\"id\":2,\"result\":{\"x\":null}}")))
           :null)
    (check "symbols gone: %PARSE-JSON %BASE64-DECODE"
           (loop for name in '("%PARSE-JSON" "%BASE64-ENCODE" "%BASE64-DECODE")
                 for s = (find-symbol name :browser-protocol)
                 count (and s (fboundp s)))
           0)
    (handler-case (b:decode-cdp-message "{\"id\":")
      (b:browser-error (c)
        (show "malformed JSON →" (type-of c))
        (check "its cause is a json-protocol condition"
               (typep (b:browser-error-cause c) 'json-protocol:json-error) t)))))

(defun demo-launch (browser)
  (section "2. Launch: chromium.launch() + new_page() (#2)"
    (para "LAUNCH-BROWSER starts the browser headless with --remote-debugging-port=0 and a"
          "temporary --user-data-dir in its own process group, reads the port from"
          "DevToolsActivePort, opens the browser websocket (ws-protocol) and attaches to the"
          "page with a flattened session: one socket, page commands carry the sessionId.")
    (show "find-browser-executable" (b:find-browser-executable))
    (show "process alive-p" (stack-process:alive-p (b:chrome-browser-process browser)))
    (show "profile" (b:chrome-browser-profile browser))
    (show "browser websocket"
          (let ((url (b:cdp-ws-url browser)))
            (subseq url 0 (or (search "/devtools/" url) (length url)))))
    (show "session id (flatten)" (format nil "~a… (~d chars)"
                                          (subseq (b:cdp-session-id browser) 0 8)
                                          (length (b:cdp-session-id browser))))
    (show "user agent" (b:eval-js browser "navigator.userAgent"))
    (check "connected" (b::cdp-connected-p browser) t)))

(defun demo-replies-by-id (browser)
  (section "3. Replies are matched by id while events arrive (#2)"
    (para "CONNECT-BROWSER sent Target.setDiscoverTargets; Chrome answers with a"
          "Target.targetCreated event per target before the reply. 0.1.0 took the first event"
          "as the reply and every later EVAL-JS read the next event (NIL, NIL, NIL). 0.2.0"
          "reads until the reply with its id and keeps the events in a bounded buffer.")
    (let ((created (loop for event = (handler-case
                                         (b:wait-for-cdp-event browser "Target.targetCreated" :timeout 0)
                                       (b:browser-timeout () nil))
                         while event
                         collect (gethash "type" (gethash "targetInfo" event)))))
      (show "Target.targetCreated events buffered" (format nil "~d: ~{~a~^, ~}" (length created) created))
      (check "at least the page" (and (member "page" created :test #'equal) t) t))
    (check "eval-js 1+1, 2+2, 3+3"
           (list (b:eval-js browser "1+1") (b:eval-js browser "2+2") (b:eval-js browser "3+3"))
           '(2 4 6))))

(defun demo-navigate (browser)
  (section "4. NAVIGATE waits for the page (#2)"
    (para "Playwright's page.goto waits for `load` by default. NAVIGATE now takes"
          ":wait-until (:load, :domcontentloaded, :commit) and :timeout; it sends Page.enable"
          "once and waits for the load event of the new loader.")
    (b:navigate browser "data:text/html,<title>fast</title><h1>hello</h1>")
    (check "default (:load): readyState" (b:eval-js browser "document.readyState") "complete")
    (check "title" (b:eval-js browser "document.title") "fast")
    (let ((slow "data:text/html,<title>slow</title><img src='http://10.255.255.1/never.png'>")
          (start (get-internal-real-time)))
      (para ""
            "A page whose image never arrives (10.255.255.1 does not answer): DOMContentLoaded"
            "fires at once, load does not.")
      (b:navigate browser slow :wait-until :domcontentloaded)
      (show ":domcontentloaded returned after" (format nil "~,1fs" (seconds-since start)))
      (check ":domcontentloaded: readyState" (b:eval-js browser "document.readyState") "interactive")
      (setf start (get-internal-real-time))
      (handler-case (progn (b:navigate browser slow :timeout 2)
                           (check ":load with :timeout 2 signals" :returned 'b:browser-timeout))
        (b:browser-timeout (c)
          (show ":load, :timeout 2 →" (format nil "~a after ~,1fs" (type-of c) (seconds-since start)))
          (show "message" c)
          (check "browser-error-url" (b:browser-error-url c) slow))))
    (b:navigate browser "about:blank")
    (b:navigate browser "about:blank#part")
    (check "#fragment (same document, no load event)" (b:eval-js browser "location.hash") "#part")
    (handler-case (progn (b:navigate browser "http://no-such-host.invalid/")
                         (check "unresolvable host signals" :returned 'b:browser-error))
      (b:browser-error (c)
        (show "unresolvable host →" (type-of c))
        (check "message names Chrome's errorText"
               (and (search "ERR_NAME_NOT_RESOLVED" (princ-to-string c)) t) t)))))

(defun demo-navigate-own-document (browser)
  (section "4b. NAVIGATE waits for its own document (#2, round 3)"
           (para "After a :commit or :domcontentloaded navigation, that page's load event is still"
                 "unread when the next NAVIGATE starts. PR #4's round-2 build took it as the next"
                 "page's load. NAVIGATE now follows the main frame's documents (Page.frameNavigated"
                 "and its loaderId) and waits for the one its own navigation committed.")
           (b:navigate browser "data:text/html,<title>one</title>" :wait-until :commit)
           (b:navigate browser "data:text/html,<title>two</title><p>2</p>")
           (check "navigate(one, :commit), then navigate(two): readyState and title"
                  (list (b:eval-js browser "document.readyState") (b:eval-js browser "document.title"))
                  '("complete" "two"))
           (let ((slow "data:text/html,<title>slow</title><img src='http://10.255.255.1/never.png'>")
                 (start nil))
             (b:navigate browser "data:text/html,<title>three</title>" :wait-until :domcontentloaded)
             (sleep 0.5)
             (para ""
                   "navigate(three, :domcontentloaded), 0.5 s, then navigate(slow, :timeout 2), whose"
                   "image never arrives: three's load event must not end it.")
             (setf start (get-internal-real-time))
             (handler-case (progn (b:navigate browser slow :timeout 2)
                             (check "navigate(slow) after three signals" :returned 'b:browser-timeout))
               (b:browser-timeout (c)
                 (show "navigate(slow) →" (format nil "~a after ~,1fs" (type-of c) (seconds-since start)))
                 (show "message" c)
                 (check "readyState of slow" (b:eval-js browser "document.readyState") "interactive")))
             (let ((titles (loop for n below 6
                                 for wait = (if (evenp n) :commit :load)
                                 do (b:navigate browser (format nil "data:text/html,<title>t~d</title>" n)
                                                :wait-until wait)
                                 when (eq wait :load)
                                   collect (b:eval-js browser "document.title"))))
               (check "six navigations, :commit and :load in turn: title after each :load"
                      titles '("t1" "t3" "t5"))))))

(defun demo-events (browser)
  (section "5. Events: ON-CDP-EVENT and WAIT-FOR-CDP-EVENT"
           (para "Handlers run on the reading thread for every event read while a command or a"
                 "wait reads. Events are also kept in a bounded buffer, and WAIT-FOR-CDP-EVENT"
                 "takes the oldest matching one from it first, so an event that came in during the"
                 "previous command is not missed. :test narrows the wait.")
           (let ((logged '()))
             (flet ((message (params) (gethash "value" (aref (gethash "args" params) 0))))
               (b:cdp-call browser "Runtime.enable")
               (b:on-cdp-event browser "Runtime.consoleAPICalled"
                               (lambda (params) (push (message params) logged)))
               (check "eval-js \"console.log('hello from the page'), 42\""
                      (b:eval-js browser "console.log('hello from the page'), 42") 42)
               (check "the handler saw the console event" (reverse logged) '("hello from the page"))
               (b:eval-js browser "setTimeout(() => console.log('later'), 300), 0")
               (check "wait-for-cdp-event: the buffered one first"
                      (message (b:wait-for-cdp-event browser "Runtime.consoleAPICalled" :timeout 5))
                      "hello from the page")
               (let* ((start (get-internal-real-time))
                      (params (b:wait-for-cdp-event browser "Runtime.consoleAPICalled"
                                                    :timeout 5
                                                    :test (lambda (p) (equal "later" (message p))))))
                 (show "the :test wait returned after" (format nil "~,1fs" (seconds-since start)))
                 (check "its message" (message params) "later"))
               (check "the handler saw both" (reverse logged) '("hello from the page" "later"))
               (b:off-cdp-event browser "Runtime.consoleAPICalled")))))

(defun demo-octets (browser)
  (section "6. Screenshot and PDF: base64 through encoding-protocol (#1)"
    (b:navigate browser "data:text/html,<title>shot</title><body style='background:%23336'><h1 style='color:white'>browser-protocol 0.2.0</h1>")
    (let ((png (b:screenshot browser)))
      (show "screenshot" (format nil "~:d bytes, ~{~d×~d~} px" (length png) (png-size png)))
      (check "PNG signature" (coerce (subseq png 0 4) 'list) '(137 80 78 71)))
    (let ((pdf (b:print-pdf browser)))
      (show "print-pdf" (format nil "~:d bytes" (length pdf)))
      (check "PDF header" (map 'string #'code-char (subseq pdf 0 5)) "%PDF-"))
    (show "dom-snapshot" (let ((dom (b:dom-snapshot browser)))
                           (subseq dom (search "<h1" dom) (+ (search "</h1>" dom) 5))))))

(defun demo-route (browser)
  (section "7. page.route: commands from an event handler (#2, round 2)"
           (para "Playwright's page.route(\"**/*\", r => r.continue_()) is Fetch.enable plus a"
                 "Fetch.requestPaused handler that sends Fetch.continueRequest. The handler runs"
                 "inside the read of whatever command or wait read the event, and Chrome pauses a"
                 "page's images in a burst, so the handlers nest. In PR #4's first build the"
                 "innermost command dropped the other replies and buffered the load event after"
                 "NAVIGATE had stopped looking: \"1 of 6 requests continued\", then BROWSER-TIMEOUT."
                 "Now whoever reads files what it reads: each reply goes to the command waiting"
                 "for its id, each event to the buffer that waits look at again after every message.")
           (let ((continued 0)
                 (page (format nil "data:text/html,<title>route</title>~{<img src='http://127.0.0.1:9/~d.png'>~}"
                               '(0 1 2 3 4 5))))
             (b:cdp-call browser "Fetch.enable"
                         (let ((h (make-hash-table :test 'equal))
                               (pattern (make-hash-table :test 'equal)))
                           (setf (gethash "urlPattern" pattern) "*"
                                 (gethash "patterns" h) (vector pattern))
                           h))
             (b:on-cdp-event browser "Fetch.requestPaused"
                             (lambda (params)
                               (let ((h (make-hash-table :test 'equal)))
                                 (setf (gethash "requestId" h) (gethash "requestId" params))
                                 (b:cdp-call browser "Fetch.continueRequest" h))
                               (incf continued)))
             (unwind-protect
                 (let ((start (get-internal-real-time)))
                   (check "navigate to a page with six images, every request routed"
                          (b:navigate browser page :timeout 10) page)
                   (show "navigate took" (format nil "~,1fs" (seconds-since start)))
                   (show "requests the handler continued" continued)
                   (check "at least the six images" (>= continued 6) t)
                   (check "readyState" (b:eval-js browser "document.readyState") "complete"))
               (b:off-cdp-event browser "Fetch.requestPaused")
               (b:cdp-call browser "Fetch.disable")))))

(defun demo-js-values (browser)
  (section "8. JavaScript values: page.evaluate (#1, round 2)"
           (para "EVAL-JS maps the RemoteObject that Runtime.evaluate returns. In PR #4's first"
                 "build a false value fell through to the RemoteObject itself, a hash-table, so"
                 "(when (eval-js b \"... !== null\") ...) always ran. A JavaScript exception came"
                 "back as a RemoteObject too, and CLICK on a missing selector returned T.")
           (b:navigate browser "data:text/html,<title>values</title><button id=go onclick=\"this.textContent='clicked'\">go</button>")
           (dolist (case `(("1 === 2" nil) ("1 === 1" t) ("undefined" nil) ("null" :null)
                           ("'s'.length" 1) ("10n ** 20n" ,(expt 10 20)) ("0/0" :nan)
                           ("-1/0" :-infinity) ("[1, false, null, 'x']" #(1 nil :null "x"))
                           ("({a: 1}).a" 1)))
             (check (format nil "eval-js ~s" (first case)) (b:eval-js browser (first case)) (second case)
                    :test #'equalp))
           (check "(when (eval-js b \"document.querySelector('#nope') !== null\") :ran)"
                  (when (b:eval-js browser "document.querySelector('#nope') !== null") :ran) nil)
           (let ((err (handler-case (b:eval-js browser "throw new Error('boom')")
                        (b:browser-error (c) (princ-to-string c)))))
             (show "eval-js \"throw new Error('boom')\"" err)
             (check "a BROWSER-ERROR with the exception's text" (and (search "Error: boom" err) t) t))
           (check "click :selector \"#missing\""
                  (handler-case (b:click browser :selector "#missing")
                    (b:browser-error () :browser-error))
                  :browser-error)
           (check "click :selector \"#go\"" (b:click browser :selector "#go") t)
           (check "its onclick ran" (b:eval-js browser "document.querySelector('#go').textContent") "clicked")))

(defun demo-threads (browser)
  (section "9. Two threads on one browser (#2, round 2)"
           (para "One thread reads the websocket at a time and files each reply for the command"
                 "that waits for its id; the others wait until it files something. Two threads,"
                 "25 EVAL-JS each, on one connection:")
           (let* ((results (make-array 2 :initial-element nil))
                  (threads (loop for i below 2
                                 collect (let ((i i))
                                           (bt2:make-thread
                                            (lambda ()
                                              (setf (aref results i)
                                                    (handler-case
                                                        (loop for k below 25
                                                              count (eql (+ (* 100 i) k)
                                                                         (b:eval-js browser (format nil "~d + ~d" (* 100 i) k))))
                                                      (error (c) (princ-to-string c)))))
                                            :name (format nil "demo ~d" i))))))
             (mapc #'bt2:join-thread threads)
             (check "right answers per thread" (coerce results 'list) '(25 25)))))

(defun demo-connect (browser)
  (section "10. CONNECT-CDP: a second client on a running browser"
           (para "connect_over_cdp: CONNECT-CDP attaches its own session; its CLOSE-BROWSER detaches"
                 "and leaves the browser running.")
           (let ((other (b:connect-cdp (b:cdp-ws-url browser))))
             (check "other client: eval-js 6*7" (b:eval-js other "6*7") 42)
             (b:close-browser other)
             (check "browser still alive after the other client closed"
                    (stack-process:alive-p (b:chrome-browser-process browser)) t)
             (check "launched client still works" (b:eval-js browser "1+1") 2))))

(defun demo-close (browser)
  (section "11. CLOSE-BROWSER"
           (let ((process (b:chrome-browser-process browser))
                 (profile (b:chrome-browser-profile browser)))
             (b:close-browser browser)
             (check "process alive-p" (stack-process:alive-p process) nil)
             (check "temporary profile exists" (and (probe-file profile) t) nil))))

(defun demo-with-browser ()
  (section "12. WITH-BROWSER and *BROWSER-BACKEND* (#3)"
           (para "WITH-BROWSER binds the backend and *BROWSER-BACKEND*; the NIL-backend methods now"
                 "use *BROWSER-BACKEND* (0.1.0 ignored it).")
           (let (process)
             (b:with-browser (w)
               (setf process (b:chrome-browser-process w))
               (b:navigate nil "data:text/html,<title>with-browser</title>")
               (check "(eval-js nil \"document.title\")" (b:eval-js nil "document.title") "with-browser"))
             (check "closed on the way out" (stack-process:alive-p process) nil))))

(defun run-demos ()
  "Run every section. → T when every check matched and no section failed."
  (setf *checks* 0 *mismatches* 0 *failed-sections* '())
  (demo-chrome-json)
  (cond
   ((null (b:find-browser-executable))
    (format t "~&~%No Chrome / Chromium / Edge found (set CHROME_PATH): sections 2-12 skipped.~%")
    (push "no browser" *failed-sections*))
   (t
    (let ((browser (b:launch-browser)))
      (unwind-protect
          (progn
            (demo-launch browser)
            (demo-replies-by-id browser)
            (demo-navigate browser)
            (demo-navigate-own-document browser)
            (demo-events browser)
            (demo-octets browser)
            (demo-route browser)
            (demo-js-values browser)
            (demo-threads browser)
            (demo-connect browser)
            (demo-close browser))
        (b:close-browser browser)))
    (demo-with-browser)))
  (format t "~&~%~d checks, ~d mismatched~@[; failed sections: ~{~a~^, ~}~]~%"
          *checks* *mismatches* (reverse *failed-sections*))
  (and (zerop *mismatches*) (null *failed-sections*)))
