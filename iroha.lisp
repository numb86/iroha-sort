(defun myFunc (arg)
  (setq arg (convert-string arg nil nil))
  (print arg))

(defun convert-string (arg target result)
  (setq target (string (first arg)))
  (cond
    ((not (search "　" target))
     (push target result)
     (setq arg (cdr arg))
     (if arg
         (convert-string arg nil result)
         (return-from convert-string result)))
    (t
      (let ((tip nil)(tail nil))
	      (setq tip (subseq target 0 (search "　" target)))
	      (setq tail (subseq target (+ 1 (search "　" target))))
       	(push tip result)
        (setq arg (cdr arg))
        (push tail arg)
        (convert-string arg nil result)))))


; (myFunc '(文文文　字　字列a bb))
(myFunc '(文文文　字　字 列 a bb))
