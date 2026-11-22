(defpackage #:harness-demos-isolation
  (:use #:cl)
  (:local-nicknames (#:pl #:cl-stack-pathlib)
                    (#:sc #:compute-protocol)
                    (#:suite #:compute-protocol/confinement-suite)
                    (#:seatbelt #:compute-backend-seatbelt)
                    (#:landlock #:compute-backend-landlock)
                    (#:proc #:process-protocol))
  (:export #:run-demos))
