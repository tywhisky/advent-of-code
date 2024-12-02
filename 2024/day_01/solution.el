(defun read-from (f)
  (with-temp-buffer
    (insert-file-contents f)
    (split-string (buffer-substring-no-properties
       (point-min)
       (point-max)) "\n" t)))

(setq input (read-from "input.txt"))

(setq parsed (mapcar (lambda (elm)
                       (mapcar #'string-to-number (split-string elm "  " )))
                     input))

(defun solution-1 ()
  (let* ((list1 '())
         (list2 '()))
    (dolist (elt parsed)
            (setq list1 (push (car elt) list1))
            (setq list2 (push (cadr elt) list2)))
    (setq list1 (sort list1 '<))
    (setq list2 (sort list2 '<))
    (-sum (seq-mapn (lambda (a b) (abs (- a b))) list1 list2))))

(solution-1)

(defun solution-2 ()
  (let* ((list1 '())
         (list2 '())
         (map (make-hash-table :test 'equal)))
    (dolist (elt parsed)
      (setq list1 (push (car elt) list1))
      (setq list2 (push (cadr elt) list2)))
    (setq map (seq-reduce (lambda (acc elm)
                            (puthash elm (1+ (or (gethash elm acc) 0)) acc)
                            acc)
                          list2 map))
    (seq-reduce (lambda (acc elm) (+ acc (* elm (or (gethash elm map) 0)))) list1 0)))

(solution-2)
