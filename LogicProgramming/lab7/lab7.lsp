(defstruct node
  value          
  count          
  left           
  right)         


(defun insert-recursive (tree value)
  (cond
    ((null tree)
     (make-node :value value :count 1))

    ((= value (node-value tree))
     (incf (node-count tree))
     tree)

    ((< value (node-value tree))
     (setf (node-left tree)
           (insert-recursive (node-left tree) value))
     tree)

    (t
     (setf (node-right tree)
           (insert-recursive (node-right tree) value))
     tree)))


(defun insert-iterative (tree value)
  (if (null tree)
      (make-node :value value :count 1)
      (let ((current tree)
            (parent nil))
        (loop while current do
             (setf parent current)
             (cond
               ((= value (node-value current))
                (incf (node-count current))
                (return tree))

               ((< value (node-value current))
                (setf current (node-left current)))

               (t
                (setf current (node-right current)))))

        (if (< value (node-value parent))
            (setf (node-left parent)
                  (make-node :value value :count 1))
            (setf (node-right parent)
                  (make-node :value value :count 1)))

        tree)))



(defun load-tree-from-file (filename &key (method :recursive))
  "method: :recursive або :iterative"
  (let ((tree nil)
        (insert-fn (if (eq method :recursive)
                       #'insert-recursive
                       #'insert-iterative)))
    (with-open-file (in filename)
      (loop for num = (read in nil 'eof)
            until (eq num 'eof)
            do (setf tree (funcall insert-fn tree num))))
    tree))


(defun print-inorder (tree)
  (when tree
    (print-inorder (node-left tree))
    (format t "~a (~a разів)~%" (node-value tree) (node-count tree))
    (print-inorder (node-right tree))))


;;; (let ((tree (load-tree-from-file "numbers.txt" :method :iterative)))
;;;   (print-inorder tree))
