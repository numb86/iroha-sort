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
    (dotimes (i (length a) nil)
      (if (= i (length b))(return-from comparison (list b a)))  ; bのほうが短いケース
      (setq a-place (search-place a i order-list))
      (setq b-place (search-place b i order-list))
      (cond
        ((equal a-place nil)
         (if (not (equal b-place nil))
             (return-from comparison (list b a))))  ; bが勝利
               ; 両方共nilのためループ続行
        ((equal b-place nil)
         (return-from comparison (list a b)))  ; aが勝利
        (t
          (cond
            ((> a-place b-place)(return-from comparison (list b a))) ; bが勝利
            ((< a-place b-place)(return-from comparison (list a b))))))) ; aが勝利
              ; 引き分けのためループ続行
    (if (= (length a)(length b))
        (return-from comparison (list a b))  ; 引き分けで終了
        (return-from comparison (list a b))))) ; aのほうが短いケース

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

; (print (comparison "アルファベット" "いろは" (list *hiragana* *katakana*)))
; (print (comparison "ルルル" "いろは" (list *hiragana* *katakana*)))
(print (comparison "ルアル" "いろは" (list *hiragana* *katakana*)))
(print (comparison "ルアル" "ルアい" (list *hiragana* *katakana*)))
(print (comparison "あおう" "いイウ" (list *hiragana* *katakana*)))
; (print (comparison "あいうえお" "あいうえお" (list *hiragana* *katakana*)))
(print (comparison "あいうえお" "あいうえあ" (list *hiragana* *katakana*)))
; (comparison "あ" "いイウ" (list *hiragana* *katakana*))

; (print (search-place "いろは" 0 (list *hiragana* *katakana*)))
; (print (search-place "アルファベット" 0 (list *hiragana* *katakana*)))

