(in-package #:harness-demos-agent)

;;; Demo 6: tui-markdown 0.1.0 — a model's markdown answer and a unified diff
;;; rendered to lines of styled runs, shown in a viewport on tui-backend-terminal's
;;; headless driver over the pure-Lisp terminal emulator (the screen text below is
;;; what the emulator holds).

(defparameter *markdown-answer*
  (format nil "~{~a~%~}"
          '("# Tree kill landed"
            ""
            "The **process-protocol 0.4.0** release adds *process groups*, so `kill :tree t`"
            "stops a shell and its jobs. See [the README](https://github.com/egao1980/process-protocol)"
            "for details; the old ~~direct-child only~~ behaviour stays the default."
            ""
            "## What changed"
            ""
            "1. `launch` / `run` take `:process-group t`"
            "2. `kill` takes `:tree t`:"
            "   - Unix: `killpg` on the child's group"
            "   - Windows: `taskkill /T /F`"
            "3. `pid` and `supports-p` are new"
            ""
            "```lisp"
            "(let ((h (launch '(\"sh\" \"-c\" \"make -j8\") :process-group t)))"
            "  (kill h :tree t))"
            "```"
            ""
            "| Capability | uiop | Notes |"
            "|:-----------|:----:|------:|"
            "| `:process-group` | yes | setpgid |"
            "| `:pid` | yes | 日本語 ok |"
            ""
            "> Killing an exited process is not an error.")))

(defparameter *answer-diff*
  (format nil "~{~a~%~}"
          '("diff --git a/src/protocol.lisp b/src/protocol.lisp"
            "index 262e345..52b257a 100644"
            "--- a/src/protocol.lisp"
            "+++ b/src/protocol.lisp"
            "@@ -86,8 +86,8 @@"
            " (defun wait (handle &key timeout)"
            "   (process-wait handle :timeout timeout))"
            " "
            "-(defun kill (handle &key force)"
            "-  (process-kill handle :force force))"
            "+(defun kill (handle &key force tree)"
            "+  (process-kill handle :force force :tree tree))"
            " "
            " (defun alive-p (handle)"
            "   (process-alive-p handle))"
            "@@ -98 +98,2 @@"
            " (defun exit-code (handle) (process-exit-code handle))"
            "+(defun pid (handle) (process-pid handle))")))

(defun %style-keys (style)
  "Non-default attributes of STYLE, as a short plist."
  (let ((out '()))
    (flet ((add (k v) (setf out (append out (list k v)))))
      (unless (eq (terminal-protocol:style-foreground style) :default)
        (add :fg (terminal-protocol:style-foreground style)))
      (when (terminal-protocol:style-bold style) (add :bold t))
      (when (terminal-protocol:style-faint style) (add :faint t))
      (when (terminal-protocol:style-italic style) (add :italic t))
      (when (terminal-protocol:style-underline style) (add :underline (terminal-protocol:style-underline style)))
      (when (terminal-protocol:style-inverse style) (add :inverse t))
      (when (terminal-protocol:style-strikethrough style) (add :strike t))
      (when (terminal-protocol:style-hyperlink style) (add :link (terminal-protocol:style-hyperlink style))))
    out))

(defun %segments (line)
  "LINE (runs) → ((text . style-keys) …), equal neighbours merged."
  (let ((out '()))
    (dolist (run (w:as-runs line))
      (let ((keys (%style-keys (w:text-run-style run)))
            (text (w:text-run-text run)))
        (when (plusp (length text))
          (if (and out (equal (cdr (first out)) keys))
              (setf (car (first out)) (concatenate 'string (car (first out)) text))
              (push (cons text keys) out)))))
    (nreverse out)))

(defun %lines-on-screen (lines cols label)
  "Show LINES in a non-wrapping viewport on a headless COLS-wide screen; print it."
  (let* ((program (w:make-widget-program (make-instance 'w:viewport :lines lines :wrap nil)))
         (tui (%headless program cols (max 3 (length lines)))))
    (unwind-protect
         (let ((first-text (find-if (lambda (s) (plusp (length (string-trim " " s))))
                                    (mapcar #'w:runs-string lines))))
           (%snap tui label :wait-for (and first-text (subseq (string-trim " " first-text) 0
                                                              (min 8 (length (string-trim " " first-text)))))))
      (ignore-errors (h:tui-stop tui)))))

(defun %fits-p (lines width)
  (every (lambda (l) (<= (w:runs-width l) width)) lines))

(defun demo-markdown ()
  (banner "Demo 6: tui-markdown 0.1.0 (markdown and diff rendering on the headless terminal)")

  (stage "6a. parse-markdown: the AST of the answer (plain lists)")
  (let ((blocks (tui-markdown:parse-markdown *markdown-answer*)))
    (note "~d blocks: ~{~(~a~)~^ ~}" (length blocks) (mapcar #'first blocks))
    (note "paragraph 1 inlines: ~s" (second (second blocks)))
    (note "table: ~s" (find :table blocks :key #'first)))

  (stage "6b. render-markdown at 60 cells, in a viewport on the headless driver")
  (let ((lines (tui-markdown:render-markdown *markdown-answer* 60)))
    (show "line count" (length lines))
    (show "every line fits 60 cells" (%fits-p lines 60))
    (%lines-on-screen lines 60 "render-markdown 60")
    (note "styles (segments) of a few lines:")
    (dolist (i (list 0 2 3 4 6))
      (when (< i (length lines))
        (note "  ~2d ~s" i (%segments (nth i lines))))))

  (stage "6c. the same answer at 34 cells: paragraphs re-wrap, the table shrinks")
  (let ((lines (tui-markdown:render-markdown *markdown-answer* 34 :show-urls nil)))
    (show "every line fits 34 cells" (%fits-p lines 34))
    (%lines-on-screen lines 34 "render-markdown 34 :show-urls nil"))

  (stage "6d. markdown-stream: committed lines never change while text streams in")
  (let* ((s (tui-markdown:make-markdown-stream :width 60))
         (chunks (loop with text = *markdown-answer*
                       for start from 0 below (length text) by 97
                       collect (subseq text start (min (length text) (+ start 97)))))
         (committed-ok t)
         (prev '()))
    (dolist (c chunks)
      (tui-markdown:markdown-stream-append s c)
      (let* ((lines (mapcar #'w:runs-string (tui-markdown:markdown-stream-lines s)))
             (n (tui-markdown:markdown-stream-committed-count s)))
        (unless (equal prev (subseq lines 0 (min (length prev) (length lines))))
          (setf committed-ok nil))
        (setf prev (subseq lines 0 n))
        (note "  +~3d chars → ~2d lines, ~2d committed" (length c) (length lines) n)))
    (tui-markdown:markdown-stream-finish s)
    (show "committed lines were always a prefix of later renders" committed-ok)
    (show "after finish: committed = all lines"
          (= (tui-markdown:markdown-stream-committed-count s)
             (length (tui-markdown:markdown-stream-lines s))))
    (show "final render = render-markdown of the whole source"
          (equal (mapcar #'w:runs-string (tui-markdown:markdown-stream-lines s))
                 (mapcar #'w:runs-string (tui-markdown:render-markdown *markdown-answer* 60))))
    (handler-case (tui-markdown:markdown-stream-append s "more")
      (tui-markdown:markdown-stream-finished (c)
        (show "append after finish signals" (type-of c)))))

  (stage "6e. render-diff-lines: unified diff text at 60 cells")
  (let ((lines (tui-markdown:render-diff-lines *answer-diff* 60)))
    (%lines-on-screen lines 60 "render-diff-lines :unified")
    (note "word-level highlight on the changed kill lines:")
    (dolist (l lines)
      (let ((text (w:runs-string l)))
        (when (search "(defun kill" text)
          (note "  ~s" (%segments l))))))

  (stage "6f. render-diff-lines :split at 100 cells, from a diff-protocol value")
  (let* ((a (format nil "(defun kill (handle &key force)~%  (process-kill handle :force force))~%"))
         (b (format nil "(defun kill (handle &key force tree)~%  (process-kill handle :force force :tree tree))~%"))
         (d (diff-protocol:compute-diff (diff-protocol:find-diff-engine :text) a b :context 3))
         (lines (tui-markdown:render-diff-lines d 100 :layout :split)))
    (%lines-on-screen lines 100 "render-diff-lines :split (diff-protocol :text value)")))
