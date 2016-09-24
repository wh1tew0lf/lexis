(defun print-frequency(alist len)
  (loop 
   for x in alist
   do (format t "\"~a\" => [~a] ~,2f%~%" (car x) (cdr x) (/ (* 100.0 (cdr x)) len))))

(defun get-frequency-alist(str) 
  (loop
   with alist = nil
   for c across str do 
   (if (assoc c alist) 
	   (incf (cdr (assoc c alist)))
	 (push (cons c 1) alist))
   finally (return-from get-frequency-alist alist)))

;;main
(cond ((= (length *posix-argv*) 2) (print-frequency 
									(get-frequency-alist (nth 1 *posix-argv*)) 
									(length (nth 1 *posix-argv*))))
	  ((= (length *posix-argv*) 3) (print-frequency 
									(if (equal (nth 2 *posix-argv*) "key") 
										(sort (copy-seq 
											   (get-frequency-alist (nth 1 *posix-argv*)) ) 
											  #'char< :key #'car)
										(sort (copy-seq 
											   (get-frequency-alist (nth 1 *posix-argv*)) ) 
											  #'> :key #'cdr))
									(length (nth 1 *posix-argv*))))
	(t (format t "~a: I need text~%Second param may be equal key - for sort by keys or anything else" (nth 0 *posix-argv*))))
