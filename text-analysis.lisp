(defun get-letter-frequency (text &optional (container nil))
  "Returns hash-table with downcased letters as keys and frequencies as values. Container optional"
  (when (null container) (setf container (make-hash-table :test 'equal)))
  (loop for chr across text do
       (let* ((lchar (char-downcase chr))
              (frequency (gethash lchar container 0)))
         (setf (gethash lchar container) (+ 1 frequency)))
     finally (return-from get-letter-frequency container)))


(defun get-n-gramm(text &optional (n 2) (container nil))
  "Returns hash-table with downcased n-grams as keys and frequencies as values"
  (when (null container) (setf container (make-hash-table :test 'equal)))
  (loop
     for chr across text
     for ngram = ""
     then (let
              ((lngram ngram) (lchar (char-downcase chr)))
            (when (alpha-char-p lchar)
              (if (= (length lngram) n)
                  (setf lngram (concatenate 'string (subseq lngram 1) (list lchar)))
                  (setf lngram (concatenate 'string lngram (list lchar))))
              (if (= (length lngram) n)
                  (let ((frequency (gethash lngram container 0)))
                    (setf (gethash lngram container) (+ 1 frequency)))))
            lngram)
     finally (return-from get-n-gramm container)))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun print-hash-table (table)
  (loop for k being the hash-keys in table using (hash-value v)
     do (format t "~A ~A ~%" k v)))

(defun print-sort-by-key (table)
  (let ((alist (hash-table-alist table))
        (cnt (loop
                for k being the hash-keys in table
                using (hash-value v)
                sum v)))

    (loop for (chr . frq) in (sort alist #'string< :key (lambda(p) (string (car p))))
      do (format t "~A ~,2F%~%" chr (/ (* 100.0 frq) cnt)))))

(defun print-sort-by-frequency (table)
  (let ((alist (hash-table-alist table))
		(cnt (loop
				for k being the hash-keys in table
				using (hash-value v)
				sum v)))

	(loop for (chr . frq) in (sort alist #'> :key #'cdr)
      do (format t "\"~A\" ~,2F%~%" chr (/ (* 100.0 frq) cnt)))))
