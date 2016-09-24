(defmacro sortbseq(str count func)
  "Sort the sequence of collection with function func from begin to count"
  `(setf (subseq ,str 0 ,count) (sort (subseq ,str 0 ,count) ,func)))

(defmacro swap(str i j) 
  "Swaps two elements from collection by indexes i and j"
  `(let ((tmp (elt ,str ,i))) 
	(setf (elt ,str ,i) (elt ,str ,j)) 
	(setf (elt ,str ,j) tmp) ,str))

(defmacro transform(str i gtf)
  "Tranform collection to next step"
  `(loop 
	  for j from 0 to ,i
	  do (when (funcall ,gtf (elt ,str j) (elt ,str ,i)) 
		   (swap ,str ,i j)
		   (sortbseq ,str ,i ,gtf)
		   (return))))

(defmacro next(str &optional (gtf #'string>) (ltf #'string<))
  "Returns next generation of collection, 
for prev step you can swap gtf and ltf functions"
  `(loop 
	 for i from 1 below (length ,str) 
	 do (when (funcall ,ltf (elt ,str i) (elt ,str (- i 1))) 
		  (transform ,str i ,gtf) (return ,str))))

(defmacro infinite-next(str &optional (gtf #'string>) (ltf #'string<))
  "Returns next generation of collection, 
for prev step you can swap gtf and ltf functions,
this function will always return you new value"
  `(loop 
	  for i from 1 below (length ,str) 
	  do (when (funcall ,ltf (elt ,str i) (elt ,str (- i 1))) 
		   (transform ,str i ,gtf) (return ,str))
		finally (return (sort ,str ,gtf))))

(defmacro list-of-transforms(test-str)
  "Tesrs the next function by run it in the loop"
  `(loop
	  with tmp = ""
	  for cnt = 0 then (+ cnt 1)
	  for str = ,test-str 
	  then (progn 
			 (format t "~a~:[, ~;~%~]" str (zerop (mod cnt 12)))
			 (next str) 
			 (if 
			  (string= tmp str) 
			  (return cnt) 
			  (setf tmp (copy-seq str))) 
			 str)))

(defmacro list-of-all-transforms(test-str)
  "Tesrs the infinite-next function by run it in the loop"
  `(loop
	  for cnt = 0 then (+ cnt 1)
	  for str = (copy-seq ,test-str) 
	  then (progn 
			 (format t "~a~:[, ~;~%~]" str (zerop (mod cnt 12)))
			 (infinite-next str) 
			 (if 
			  (string= ,test-str str) 
			  (return cnt)) 
			 str)))

(defun main()
  (list-of-all-transforms "1234"))

(sb-ext:save-lisp-and-die "my-program.exe" :executable t :toplevel #'main)
