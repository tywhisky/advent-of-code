(defun read-from (f)
  (with-temp-buffer
    (insert-file-contents f)
    (split-string (buffer-substring-no-properties
       (point-min)
       (point-max)) "\n" t)))

(setq parsed
      (mapcar (lambda (row)
                (mapcar #'string-to-number
                        (split-string row " " t)))
              (read-from "input.txt")))

(defun check-row (row)
  (let* ((prev (car row))
         (distance (mapcar (lambda (elt)
                             (let ((d (- elt prev)))
                               (setq prev elt)
                               d)) (cdr row))))
    (and (seq-every-p (lambda (x) (<= (abs x) 3)) distance)
         (or (seq-every-p (lambda (x) (> x 0)) distance)
             (seq-every-p (lambda (x) (< x 0)) distance)))))
    

(defun solution-of-part-one ()
  (let ((result 0))
    (dolist (row parsed)
      (when (check-row row) (setq result (1+ result))))
    result))

(solution-of-part-one)

(setq unsafe-rows (seq-reduce (lambda (acc row)
                                (if (not (check-row row))
                                    (cons row acc)
                                  acc))
                              parsed
                              '()))

(defun remove-nth (n list)
  "Remove the nth element of a list."
  (if (> n (length list))
      list
    (append (cl-subseq list 0 n)
            (cl-subseq list (1+ n)))))

(defun remove-one (lst)
  (let ((result '())) 
    (dotimes (i (length lst))
      (push (remove-nth i lst) result))
    (nreverse result)))

(remove-one '(1 2 3))

(defun solution-of-part-two ()
  (let ((result 0))
    (dolist (row unsafe-rows)
      (let* ((expand-rows (remove-one row))
             (t-or-nil (mapcar (lambda (ur)
                                 (check-row ur))
                               expand-rows)))
        (if (seq-some (lambda (x) (eq x t)) t-or-nil)
            (setq result (1+ result)))))
    result))

(solution-of-part-two) ;;45
        
(seq-some (lambda (x) (eq x t)) '(nil nil t))
