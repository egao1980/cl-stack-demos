;;;; Inspired by jpadilla/pyjwt README usage — HS256 encode/decode/exp.
;;;;
;;;; GAP: (expired-p token :verify t …) on an already-expired token signals
;;;; JOSE/ERRORS:JWT-CLAIMS-EXPIRED instead of returning T.
;;;; See egao1980/cl-stack-jwt issue (logged in ISSUES.md). Demo only asserts
;;;; the non-expired path + expired-p with :verify nil.
(in-package #:cl-stack-demos)

(defun run-pyjwt-hs256 ()
  (let* ((key (ironclad:ascii-string-to-byte-array "secret-key-123456789012345678901234"))
         (exp (+ (stack-jwt:unix-time) 3600))
         (token (stack-jwt:encode :hs256 key
                                  `(("some" . "payload") ("exp" . ,exp))))
         (claims (stack-jwt:decode :hs256 key token)))
    (format t "~&; token=~A~%" token)
    (format t "~&; claims=~S expired=~A~%"
            claims (stack-jwt:expired-p token :verify t :algorithm :hs256 :key key))
    (assert (string= (cdr (assoc "some" claims :test #'string=)) "payload"))
    (assert (not (stack-jwt:expired-p token :verify t :algorithm :hs256 :key key)))
    (let* ((old (stack-jwt:encode :hs256 key
                                  `(("some" . "old") ("exp" . ,(- (stack-jwt:unix-time) 10)))))
           ;; :verify nil — :verify t raises (upstream bug; do not catch/workaround)
           (expired (stack-jwt:expired-p old :verify nil)))
      (format t "~&; expired(verify nil)=~A~%" expired)
      (assert expired)
      t)))

(register-app "pyjwt-hs256"
              :title "PyJWT HS256 encode/decode/exp"
              :upstream "https://github.com/jpadilla/pyjwt"
              :fn #'run-pyjwt-hs256)
