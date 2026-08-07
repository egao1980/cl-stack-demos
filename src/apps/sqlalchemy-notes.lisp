;;;; Inspired by sqlalchemy ORM tutorial — User/Note CRUD.
(in-package #:cl-stack-demos)

(defun run-sqlalchemy-notes ()
  (asdf:load-system "sql-backend-sqlite3")
  (asdf:load-system "sql-query-sqlite3")
  (asdf:load-system "sql-orm")
  ;; Fresh models each run — avoid redefinition issues by unique package? use existing.
  (unless (find-class 'demo-user nil)
    (stack-sql-orm:defmodel demo-user ()
      (id :integer :primary-key t :autoincrement t)
      (name :text :not-null t)
      (email :text)
      (:table demo_users)
      (:has-many notes demo-note :key user-id)
      (:compute label (self)
        (format nil "~A <~A>" (name self) (or (email self) "")))))
  (unless (find-class 'demo-note nil)
    (stack-sql-orm:defmodel demo-note ()
      (id :integer :primary-key t :autoincrement t)
      (user-id :integer :not-null t)
      (title :text :not-null t)
      (body :text)
      (:table demo_notes)
      (:belongs-to user demo-user :key user-id)))
  (stack-sql-orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
    (stack-sql-orm:ensure-schema c 'demo-user)
    (stack-sql-orm:ensure-schema c 'demo-note)
    (let* ((u (stack-sql-orm:persist
               (make-instance 'demo-user :name "ada" :email "a@x")))
           (n (stack-sql-orm:persist
               (make-instance 'demo-note
                              :user-id (id u)
                              :title "hello"
                              :body "world")))
           (found (stack-sql-orm:find-instance 'demo-user (id u)))
           (notes (stack-sql-orm:select-instances
                   'demo-note :where (:= :user-id (id u)))))
      (format t "~&; user=~A note=~A count=~A~%"
              (label found) (title n) (length notes))
      (assert (string= (name found) "ada"))
      (assert (= 1 (length notes)))
      (stack-sql-orm:destroy n)
      (stack-sql-orm:destroy u)
      t)))

(register-app "sqlalchemy-notes"
              :title "SQLAlchemy-shaped User/Note ORM CRUD"
              :upstream "https://github.com/sqlalchemy/sqlalchemy"
              :fn #'run-sqlalchemy-notes)
