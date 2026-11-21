(in-package #:harness-demos-code)

;;; The sampling preset every live model call sends (HARNESS_DEMOS_MODE).
;;; Recommended for Qwen3.8-Flash-Next / Inkling on llama.cpp (Unsloth
;;; Studio). Keys are wire names: llama.cpp's OpenAI endpoint reads top_k,
;;; min_p, presence_penalty, repeat_penalty (its name for repetition_penalty)
;;; and chat_template_kwargs, which the Qwen chat template sees as Jinja
;;; variables (enable_thinking; preserve_thinking keeps earlier turns'
;;; reasoning, which llm-protocol-openai sends back as reasoning_content).
;;; The same file is in each harness set (agent, code, image, supervisor).

(defparameter +sampling-presets+
  '((:thinking . "{\"temperature\":1.0,\"top_p\":0.95,\"top_k\":20,\"min_p\":0.0,\"presence_penalty\":0.0,\"repeat_penalty\":1.0,\"chat_template_kwargs\":{\"enable_thinking\":true,\"preserve_thinking\":true}}")
    (:instruct . "{\"temperature\":0.7,\"top_p\":0.8,\"top_k\":20,\"min_p\":0.0,\"presence_penalty\":1.5,\"repeat_penalty\":1.0,\"chat_template_kwargs\":{\"enable_thinking\":false}}"))
  "HARNESS_DEMOS_MODE → the JSON fields every live request carries.")

(defun sampling-mode ()
  "HARNESS_DEMOS_MODE: :THINKING (the default, also when unset or empty) or
:INSTRUCT; anything else is an error."
  (let ((m (string-downcase (string-trim " " (or (uiop:getenv "HARNESS_DEMOS_MODE") "")))))
    (cond ((member m '("" "thinking") :test #'string=) :thinking)
          ((string= m "instruct") :instruct)
          (t (error "HARNESS_DEMOS_MODE must be thinking or instruct, not ~s" m)))))

(defun sampling-json ()
  "The preset's JSON text."
  (cdr (assoc (sampling-mode) +sampling-presets+)))

(defun sampling-fields ()
  "The preset as an alist (wire-name . decoded JSON value), in key order."
  (let ((h (json:decode (sampling-json))) (out '()))
    (maphash (lambda (k v) (push (cons k v) out)) h)
    (sort out #'string< :key #'car)))

(defun sampling-description ()
  "One line for the output, next to MODEL THAT RAN."
  (format nil "SAMPLING PRESET SENT: ~(~a~) (HARNESS_DEMOS_MODE) ~a" (sampling-mode) (sampling-json)))

(defun sampling-llm-settings (&key max-tokens extra)
  "An LLM-SETTINGS with the preset: temperature and top_p as its slots, the
other fields in :EXTRA (copied into the body by llm-protocol-openai), then
EXTRA (a plist) after them."
  (let ((temperature nil) (top-p nil) (rest '()))
    (loop for (k . v) in (sampling-fields)
          do (cond ((string= k "temperature") (setf temperature v))
                   ((string= k "top_p") (setf top-p v))
                   (t (setf rest (list* v k rest)))))
    (llm:make-llm-settings :temperature temperature :top-p top-p :max-tokens max-tokens
                           :extra (append (reverse rest) extra))))

(defun sampling-flags ()
  "The preset as cl-stack-code 0.9.0 --model-param KEY=VALUE flags (VALUE as
JSON)."
  (loop for (k . v) in (sampling-fields)
        append (list "--model-param" (format nil "~a=~a" k (json:encode v)))))
