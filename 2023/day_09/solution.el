(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(setq parse
      (mapcar (lambda (line)
                (mapcar #'string-to-number (split-string line " " t)))
              (read-file "test.txt")))

(message "Lines in the file: %s" parse)
