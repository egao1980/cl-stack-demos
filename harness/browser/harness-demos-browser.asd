(defsystem "harness-demos-browser"
  :version "0.1.0"
  :description "Demo of browser-protocol 0.2.0 (browser-protocol#1, #2, #3): launch headless Chrome with LAUNCH-BROWSER / WITH-BROWSER, NAVIGATE that waits for the load event (:wait-until :load / :domcontentloaded / :commit, :timeout), CDP replies matched by id while events arrive, ON-CDP-EVENT / WAIT-FOR-CDP-EVENT, screenshot / PDF base64 through encoding-protocol, Chrome's JSON (false, null) through json-protocol, a failed navigation as BROWSER-ERROR, CONNECT-CDP to a running browser, and CLOSE-BROWSER stopping the process and removing the profile"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "browser-protocol" "0.2.0") "browser-protocol/launch"
               "process-protocol" "json-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
