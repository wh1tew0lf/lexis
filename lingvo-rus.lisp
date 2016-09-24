(ql:quickload 'cl-ppcre)

(defclass lingvo-rus()
  ((version 
	:initform "0.0.2" 
	:reader version 
	:allocation :class)
   (caching 
	:initform nil 
	:accessor caching 
	:allocation :class)
   (cache 
	:initform nil 
	:accessor cache 
	:allocation :class)
   (vowel 
	:initform "аеиоуыэюя" 
	:reader vowel 
	:allocation :class)
   (perfectgeround 
	:initform "((ив|ивши|ившись|ыв|ывши|ывшись)|((?<=[ая])(в|вши|вшись)))$" 
	:reader perfectgeround 
	:allocation :class)
   (reflexive 
	:initform "(с[яь])$" 
	:reader reflexive 
	:allocation :class)
   (adjective 
	:initform "(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ему|ем|им|ым|ому|ом|его|ого|еых|ых|их|ую|юю|ая|яя|ою|ею)$" 
	:reader adjective 
	:allocation :class)
   (participle 
	:initform "((ивш|ывш|ующ)|((?<=[ая])(ем|нн|вш|ющ|щ)))$" 
	:reader participle 
	:allocation :class)
   (verb 
	:initform "((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ило|ил|ыло|ыл|им|ым|ено|ят|ует|уют|ены|ить|ыть|ит|ыт|ишь|ую|ю)|((?<=[ая])(ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно)))$" 
	:reader verb 
	:allocation :class)
   (noun 
	:initform "(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|и|ы|ь|ию|ью|ю|ия|ья|я)$" 
	:reader noun 
	:allocation :class)
   (rvre 
	:initform "^(.*?[аеиоуыэюя])(.*)$" 
	:reader rvre 
	:allocation :class)
   (derivational 
	;:initform "[^аеиоуыэюя][аеиоуыэюя]+[^аеиоуыэюя]+[аеиоуыэюя].*(?<=о)сть?$" 
	:initform "[^аеиоуыэюя]+[аеиоуыэюя].*(?<=о)сть?$" 
	:reader derivational 
	:allocation :class)))

(defun check (subject pattern &optional (replace ""))
  (let ((tmp (cl-ppcre:regex-replace pattern subject replace))) 
	(values tmp (string/= tmp subject))))

(defun match (subject pattern)
  (cl-ppcre:scan-to-strings pattern subject))

;;Do each of steps 1, 2, 3 and 4. 

;;Step 1: Search for a PERFECTIVE GERUND ending. 
;;If one is found remove it, and that is then the end of step 1. 
;;Otherwise try and remove a REFLEXIVE ending, 
;;and then search in turn for (1) an ADJECTIVAL, (2) a VERB or (3) a NOUN ending. 
;;As soon as one of the endings (1) to (3) is found remove it, and terminate step 1. 

;;Step 2: If the word ends with и (i), remove it. 

;;Step 3: Search for a DERIVATIONAL ending in R2 (i.e. the entire ending must lie in R2), 
;;and if one is found, remove it. 

;;Step 4: (1) Undouble н (n), or, (2) if the word ends with a SUPERLATIVE ending, 
;;remove it and undouble н (n), or (3) if the word ends ь (') (soft sign) remove it.


(defgeneric stem-word (obj word))
(defmethod stem-word ((obj lingvo-rus) word)
  (setf word (cl-ppcre:regex-replace-all "ё" (string-downcase word) "е"))
  (let
	  ((start "")
	   (rv ""))
	(multiple-value-bind (res matches) 
		(cl-ppcre:scan-to-strings (rvre obj) word)
	  (if res (progn 
				(setf start (elt matches 0))
				(setf rv (elt matches 1)))
		  (return-from stem-word word)))
	(unless rv (return-from stem-word word))
	(multiple-value-bind (new-word res)
		(check rv (perfectgeround obj))
	  (if res
		  (setf rv new-word)
		  (progn
			(setf rv (check rv (reflexive obj)))
			(multiple-value-bind (new-word res)
				(check rv (adjective obj))
			  (if res 
				  (setf rv (check new-word (participle obj)))				  
				  (multiple-value-bind (new-word res)
					  (check rv (verb obj))
					(if res 
						(setf rv new-word)
						(setf rv (check rv (noun obj))))))))))
	(setf rv (check rv "и$"))
	(if (match rv (derivational obj))
		(setf rv (check rv "ость?$")))
	(multiple-value-bind (new-word res)
		(check rv "ь$")
	  (if res
		  (setf rv new-word)
		  (progn
			(setf rv (check rv "ейше?$"))
			(setf rv (check rv "нн$" "н")))))
	  (concatenate 'string start rv)))

;;Не годится, так как нужно удалить повторяющиеся
;;(with-open-file (fout "/home/wh1/words/roots.txt" :direction :output)
;;  (with-open-file (fin "/home/wh1/words/sorted.txt")
;;	(loop for line = (read-line fin nil)
;;	   while line do (format fout "~a~%" (string-upcase (stem-word *lr* line))))))
