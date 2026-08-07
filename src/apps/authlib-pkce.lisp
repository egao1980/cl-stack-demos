;;;; Inspired by authlib OAuth2 PKCE client — authorize URI + S256 challenge.
(in-package #:cl-stack-demos)

(defun run-authlib-pkce ()
  (let* ((pkce (stack-oauth2:make-pkce))
         (auth (stack-oauth2:make-oauth2-auth
                :authorize-url "https://as.example/authorize"
                :token-url "https://as.example/token"
                :client-id "cid"
                :redirect-uri "https://app/cb"
                :scope "openid profile"))
         (uri (stack-oauth2:oauth2-authorization-uri auth :pkce t)))
    (format t "~&; method=~A challenge-len=~A~%"
            (stack-oauth2:pkce-method pkce)
            (length (stack-oauth2:pkce-challenge pkce)))
    (format t "~&; authorize-uri=~A~%" uri)
    (assert (string= (stack-oauth2:pkce-method pkce) "S256"))
    (assert (>= (length (stack-oauth2:pkce-verifier pkce)) 43))
    (assert (search "https://as.example/authorize" uri))
    (assert (or (search "code_challenge" uri)
                (search "client_id" uri)))
    t))

(register-app "authlib-pkce"
              :title "Authlib-shaped OAuth2 PKCE authorize URI"
              :upstream "https://github.com/authlib/authlib"
              :fn #'run-authlib-pkce)
