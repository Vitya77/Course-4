(defun put (symbol property value)
  (setf (get symbol property) value))

(setq ME 'ME)
(setq FRIEND 'FRIEND)

(put 'ME 'name "Viktor")
(put 'ME 'age 22)
(put 'ME 'activity "Student, programmer")
(put 'ME 'skills '(Python Lisp React Native))
(put 'ME 'interests '(AI music travel))

(put 'FRIEND 'name "Oleksandr")
(put 'FRIEND 'age 23)
(put 'FRIEND 'activity "Designer")
(put 'FRIEND 'skills '(Photoshop Figma React))
(put 'FRIEND 'interests '(movies travel sport))

(defun get-prop (person prop)
  (get person prop))

(defun print-info (person)
  (format t "~%Name: ~A" (get person 'name))
  (format t "~%Age: ~A" (get person 'age))
  (format t "~%Activity: ~A" (get person 'activity))
  (format t "~%Skills: ~A" (get person 'skills))
  (format t "~%Interests: ~A~%" (get person 'interests)))

(defun set-prop (person prop value)
  (put person prop value)
  (format t "~%Property ~A updated for ~A." prop person))

(defun older (p1 p2)
  (let ((age1 (get p1 'age))
        (age2 (get p2 'age)))
    (cond ((> age1 age2) (format t "~%~A is older than ~A." (get p1 'name) (get p2 'name)))
          ((< age1 age2) (format t "~%~A is older than ~A." (get p2 'name) (get p1 'name)))
          (t (format t "~%They are the same age.")))))

(defun common-skills (p1 p2)
  (let* ((skills1 (get p1 'skills))
         (skills2 (get p2 'skills))
         (common (intersection skills1 skills2)))
    (if common
        (format t "~%Common skills: ~A" common)
        (format t "~%No common skills."))))

(format t "~%Info about ME:")
(print-info 'ME)

(format t "~%Info about FRIEND:")
(print-info 'FRIEND)

(format t "~%Comparison:")
(older 'ME 'FRIEND)
(common-skills 'ME 'FRIEND)

(format t "~%Changing property:")
(set-prop 'FRIEND 'activity "UX/UI designer")
(print-info 'FRIEND)
