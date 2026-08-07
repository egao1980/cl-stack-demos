;;;; Inspired by jpadilla/pyjwt README usage — HS256 encode/decode/exp.
(in-package #:cl-stack-demos)

(defun run-pyjwt-hs256 ()
  (let* ((key (ironclad:ascii-string-to-byte-array "secret"))
         (exp (+ (stack-jwt:unix-time) 3600))
         (token (stack-jwt:encode :hs256 key
                                  `(("some" . "payload") ("exp" . ,exp))))
         (claims (stack-jwt:decode :hs256 key token)))
    (format t "~&; token=~A~%" token)
    (format t "~&; claims=~S expired=~A~%"
            claims (stack-jwt:expired-p token :verify t :algorithm :hs256 :key key))
    (assert (string= (cdr (assoc "some" claims :test #'string=)) "payload"))
    (assert (not (stack-jwt:expired-p token :verify t :algorithm :hs256 :key key)))
    ;; expired token
    (let* ((old (stack-jwt:encode :hs256 key
                                  `(("some" . "old") ("exp" . ,(- (stack-jwt:unix-time) 10)))))
           (expired (stack-jwt:expired-p old :verify t :algorithm :hs256 :key key)))
      (assert expired)
      t)))

(register-app "pyjwt-hs256"
              :title "PyJWT HS256 encode/decode/exp"
              :upstream "https://github.com/jpadilla/pyjwt"
              :fn #'run-pyjwt-hs256)
