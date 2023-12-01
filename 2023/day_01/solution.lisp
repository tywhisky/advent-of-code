(defun sum-of-two-digits-in-lines (file-path)
  "Read the contents of a file, extract two digits from each line, and calculate the sum."
  (interactive "fEnter file path: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((file-contents (buffer-string))
           (lines (split-string file-contents "\n" t))
           (total-sum 0))
      (dolist (line lines)
        (let ((line-digits (replace-regexp-in-string "[^0-9]" "" line)))
          (when (>= (length line-digits) 2)
            (let* ((first-digit (string-to-number (substring line-digits 0 1)))
                   (last-digit (string-to-number (substring line-digits -1)))
                   (two-digit-number (+ (* 10 first-digit) last-digit)))
              (setq total-sum (+ total-sum two-digit-number))
              (message "Line: %s, Two-Digit Number: %d" line two-digit-number)))))
      (message "Total Sum of Two-Digit Numbers: %d" total-sum))))

;; 例子：读取文件并提取每行的两个数字组成的两位数字，计算总和
(sum-of-two-digits-in-lines "input.txt")
