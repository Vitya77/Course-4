;; Example 1
;; Using of mapcar with lambda function (map analogy from Python)

(let ((numbers '(1 2 3 4 5)))
  (print (mapcar (lambda (x) (* x 2))
                 numbers)))


;; Example 2
;; Using of planning of another function (closure example)
(defun make-multiplier (factor)
  (lambda (x)
    (* x factor)))

(let ((double (make-multiplier 2))
      (triple (make-multiplier 3)))
  (print (list (funcall double 10)
               (funcall triple 10))))


;; Example 3
;; Filter analogy

(let ((students '((:name "Ivan"   :grade 90)
                  (:name "Oleh"   :grade 75)
                  (:name "Maria"  :grade 82)
                  (:name "Sofia"  :grade 60))))
  (print (remove-if-not
          (lambda (student)
            (>= (getf student :grade) 80))
          students)))
