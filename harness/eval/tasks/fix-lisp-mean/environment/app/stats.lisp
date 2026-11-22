;;;; stats.lisp: tiny statistics helpers.

(defun mean (numbers)
  "The arithmetic mean of the non-empty list NUMBERS."
  (/ (reduce #'+ numbers) (1+ (length numbers))))

(defun spread (numbers)
  "The largest minus the smallest of NUMBERS."
  (- (reduce #'max numbers) (reduce #'min numbers)))
