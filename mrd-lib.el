(defun mrd-filter-mapcar (fun lst)
  "Like mapcar, but only true values are saved."
  ;; XXX: Could be faster.
  (apply 'append
	 (mapcar (lambda (x)
		   (let ((res (funcall fun x)))
		     (if res (list res) (list))))
		 lst)))

(defun mrd-filter (pred lst)
  "Return all the elements of lst that satisfy predicate pred."
  ;; XXX: Could be faster.
  (apply 'append
	 (mapcar (lambda (x)
		   (if (funcall pred x)
		       (list x)
		     (list)))
		 lst)))

(defun mrd-remove (pred lst)
  "Return lst without the elements of lst that satisfy predicate pred."
  ;; XXX: Could be faster.
  (apply 'append
	 (mapcar (lambda (x)
		   (if (funcall pred x)
		       (list)
		     (list x)))
		 lst)))

(defun mrd-string-prefix-p (s1 s2)
  "Return t iff s1 is a prefix of s2"
  (and (<= (length s1) (length s2))
       (string-equal s1 (substring s2 0 (length s1)))))

(provide 'mrd-lib)
