(defun read-from (f)
  (with-temp-buffer
    (insert-file-contents f)
    (split-string (buffer-substring-no-properties
       (point-min)
       (point-max)) "\n" t)))

(setq parsed (read-from "input.txt"))

(defun sum-mul-instructions (input-string)
  "Calculate the sum of all valid mul(X,Y) instructions in INPUT-STRING."
  (let ((regex "mul(\\([0-9]\\{1,3\\}\\),\\s-*\\([0-9]\\{1,3\\}\\))")
        (total 0))
    (while (string-match regex input-string)
      (let ((x (string-to-number (match-string 1 input-string)))
            (y (string-to-number (match-string 2 input-string))))
        (setq total (+ total (* x y))))
      (setq input-string (substring input-string (match-end 0))))
    total))

(defun part-one ()
  (seq-reduce (lambda (acc elt)
                (+ acc (sum-mul-instructions elt)))
              parsed
              0))

(part-one)

(defun calculate-enabled-multiplications (input-string)
  "Calculate the sum of all enabled mul(X,Y) instructions in INPUT-STRING."
  (let ((mul-regex "mul(\\([0-9]\\{1,3\\}\\),\\s-*\\([0-9]\\{1,3\\}\\))")
        (do-regex "do()")
        (dont-regex "don't()")
        (total 0)
        (enabled t))  ;; Start with multiplications enabled
    (while (not (string-empty-p input-string))
      (cond
       ;; Check for 'mul' instructions
       ((string-match mul-regex input-string)
        (when enabled  ;; Only process if enabled
          (let ((x (string-to-number (match-string 1 input-string)))
                (y (string-to-number (match-string 2 input-string))))
            ;; Calculate the product and add to total
            (setq total (+ total (* x y)))))
        ;; Update input-string to continue searching
        (setq input-string (substring input-string (match-end 0))))

       ;; Check for 'do()' instruction
       ((string-match do-regex input-string)
        (setq enabled t)  ;; Enable multiplications
        (setq input-string (substring input-string (match-end 0))))

       ;; Check for 'don't()' instruction
       ((string-match dont-regex input-string)
        (setq enabled nil)  ;; Disable multiplications
        (setq input-string (substring input-string (match-end 0))))

       ;; If no matches found, break out of loop
       (t
        ;; Move past one character to avoid infinite loop on unmatched characters
        (setq input-string (substring input-string 1)))))

    total))

(defun part-two ()
  (seq-reduce (lambda (acc elt)
                (+ acc (calculate-enabled-multiplications elt)))
              parsed
              0))

(part-two)
(calculate-enabled-multiplications "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
