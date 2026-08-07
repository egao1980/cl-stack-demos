;;;; Inspired by pallets/click examples/naval — nested ship/mine groups.
(in-package #:cl-stack-demos)

(defun run-click-naval ()
  (asdf:load-system "cli-backend-clingon")
  (let* ((ship-new
           (stack-cli:make-command
            :name "new"
            :description "Creates a new ship."
            :handler (lambda (opts free)
                       (declare (ignore opts))
                       (format t "Created ship ~A~%" (first free)))))
         (ship-move
           (stack-cli:make-command
            :name "move"
            :description "Moves SHIP to X,Y."
            :options (list (stack-cli:make-option
                            :name "speed" :long "speed"
                            :kind :integer :default 10 :key :speed
                            :help "Speed in knots."))
            :handler (lambda (opts free)
                       (format t "Moving ship ~A to ~A,~A with speed ~A~%"
                               (first free) (second free) (third free)
                               (stack-cli:get-option opts :speed 10)))))
         (ship
           (stack-cli:make-command
            :name "ship"
            :description "Manages ships."
            :subcommands (list ship-new ship-move)))
         (mine-set
           (stack-cli:make-command
            :name "set"
            :description "Sets a mine."
            :handler (lambda (opts free)
                       (declare (ignore opts))
                       (format t "Set mine at ~A,~A~%" (first free) (second free)))))
         (mine
           (stack-cli:make-command
            :name "mine"
            :description "Manages mines."
            :subcommands (list mine-set)))
         (cli
           (stack-cli:make-command
            :name "naval"
            :description "Naval Fate (click naval port)."
            :version "0.1.0"
            :subcommands (list ship mine))))
    (stack-cli:run cli :argv '("ship" "new" "Destiny"))
    (stack-cli:run cli :argv '("ship" "move" "Destiny" "10" "20" "--speed" "15"))
    (stack-cli:run cli :argv '("mine" "set" "5" "6"))
    t))

(register-app "click-naval"
              :title "Click naval nested subcommands"
              :upstream "https://github.com/pallets/click/tree/main/examples/naval"
              :fn #'run-click-naval)
