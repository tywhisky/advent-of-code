(defun read-from (f)
  (with-temp-buffer
    (insert-file-contents f)
    (split-string (buffer-substring-no-properties
       (point-min)
       (point-max)) "\n" t)))

(defun make-matrix-map (matrix)
  (let ((map (make-hash-table :test #'equal)))
    (dotimes (i (length matrix))
      (dotimes (j (length (nth i matrix)))
        (puthash (list i j) (nth j (nth i matrix)) map)))
    map))

(setq parsed
       (mapcar
        (lambda (row)
          (split-string row "" t))
        (read-from "input.txt")))

(setq map (make-matrix-map parsed))

(defun part-one ()
  (let ((result 0))
    (dotimes (i (length parsed))
      (dotimes (j (length parsed))
        (let ((ll (expand i j)))
          (print ll)
          (cond
           ((member (list "X" "M" "A" "S") ll) (setq result (1+ result)))
           ((member (list "S" "A" "M" "X") ll) (setq result (1+ result)))
           (t nil)))))
    result))

(part-one)
              
(defun expand (x y)
  (let ((coor (list
                (list (list x y) (list x (1+ y)) (list x (+ 2 y)) (list x (- y 3)))
                (list (list x y) (list x (1- y)) (list x (- y 2)) (list x (- y 3)))
                (list (list x y) (list (1+ x) y) (list (+ x 2) y) (list (+ x 3) y))
                (list (list x y) (list (1- x) y) (list (- x 2) y) (list (- x 3) y))
                (list (list x y) (list (1+ x) (1+ y)) (list (+ x 2) (+ y 2)) (list (+ x 3) (+ y 3)))
                (list (list x y) (list (1- x) (1- y)) (list (- x 2) (- y 2)) (list (- x 3) (- y 3)))
                (list (list x y) (list (1+ x) (1- y)) (list (+ x 2) (- y 2)) (list (+ x 3) (- y 3))))))
    (mapcar (lambda (row)
              (mapcar (lambda (item) (gethash item map)) row))
            coor)))
        


(cons 2 '(3 4))
(mapconcat 'identity (list "b" "a") "")
(eq nil (member "t" (list "a" "b")))
