;; 定义一个函数来计算斐波那契数列
(defun fibonacci (n)
  "Calculate the nth Fibonacci number."
  (if (< n 2)
      n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

;; 测试斐波那契函数
(message "Fibonacci of 0: %d" (fibonacci 0))
(message "Fibonacci of 1: %d" (fibonacci 1))
(message "Fibonacci of 5: %d" (fibonacci 5))
(message "Fibonacci of 10: %d" (fibonacci 10))
