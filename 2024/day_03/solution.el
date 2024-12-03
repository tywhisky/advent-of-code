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

(sum-mul-instructions "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5)")
