(defparameter *hiragana* "あいうえお")
(defparameter *katakana* "アイウエオ")

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

(defun comparison (a b order-list)
  (let ((a-place nil)(b-place nil))
    (dotimes (i 3 nil)
      (setq a-place (search-place a i order-list))
      (setq b-place (search-place b i order-list))
      (cond
        ((equal a-place nil)
         (if (equal b-place nil)
             (print "両方共nil")
             (print "bの勝ち")))
        ((equal b-place nil)
         (print "aの勝ち"))
        (t
          (cond
            ((= a-place b-place)(print "引き分け"))
            ((> a-place b-place)(print "bの勝ち"))
            ((< a-place b-place)(print "aの勝ち"))))))))

(defun search-place (word num order-list)
  (dotimes (i (length order-list) nil)
    (let ((target nil)(place nil))
      (setq target (subseq word num (+ 1 num)))
      (setq place (search target (nth i order-list)))
      (if place
          (return-from search-place place))))
  (return-from search-place nil))


; (myFunc '(文文文　字　字列a bb))
; (myFunc '(文文文　字　字 列 a bb))

; (comparison "アルファベット" "いろは" (list *hiragana* *katakana*))
; (comparison "ルルル" "いろは" (list *hiragana* *katakana*))
; (comparison "ルアル" "いろは" (list *hiragana* *katakana*))
(comparison "あおう" "いイウ" (list *hiragana* *katakana*))

; (print (search-place "いろは" 0 (list *hiragana* *katakana*)))
; (print (search-place "アルファベット" 0 (list *hiragana* *katakana*)))

