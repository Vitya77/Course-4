(defun list-all-fractions-in-range (lst a b)
  (cond
    ((null lst) t)
    ((or (integerp (car lst))
         (< (car lst) a)
         (> (car lst) b))
     nil)
    (t (list-all-fractions-in-range (cdr lst) a b))))


(defun sum-first-m (lst m)
  (cond
    ((= m 0) 0)
    ((null lst) 0)
    (t (+ (car lst) (sum-first-m (cdr lst) (1- m))))))

(defun drop-m (lst m)
  (cond
    ((= m 0) lst)
    ((null lst) nil)
    (t (drop-m (cdr lst) (1- m)))))

(defun group-sums (lst m)
  (cond
    ((null lst) nil)
    ((< (length lst) m) nil)
    (t (cons (sum-first-m lst m)
             (group-sums (drop-m lst m) m)))))


(defun my-reverse (lst)
  (labels ((rev (l acc)
             (if (null l)
                 acc
               (rev (cdr l) (cons (car l) acc)))))
    (rev lst nil)))

(defun my-sum (lst)
  (if (null lst)
      0
    (+ (car lst) (my-sum (cdr lst)))))

(defun transform-sublist (lst)
  (append (my-reverse lst) (list (my-sum lst))))

(defun transform-sublists (lst)
  (cond
    ((null lst) nil)
    (t (cons (transform-sublist (car lst))
             (transform-sublists (cdr lst))))))


(defun run-tests ()
  (format t "~%Task 1~%")
  (format t "Test 1: ~A~%" (list-all-fractions-in-range '(1.5 2.7 3.14) 1 4))
  (format t "Test 2: ~A~%" (list-all-fractions-in-range '(1.5 2 3.1) 1 4))

  (format t "~%Task 2~%")
  (format t "Test 3: ~A~%" (group-sums '(1 2 3 4 5 6) 2))
  (format t "Test 4: ~A~%" (group-sums '(1 2 3 4 5 6) 3))

  (format t "~%Task 3~%")
  (format t "Test 5: ~A~%" (transform-sublists '((1 2 3) (4 5))))
  (format t "Test 6: ~A~%" (transform-sublists '((10 20) (3 3 3) (7))))

  t)


(run-tests)
