(format t "(iroha '(単語　単語)) という形で入力してください。~%")
(format t "~%")
(format t "例~%")
(format t "(iroha '(いしだ　はらだ　いいだ))~%")
(format t "結果~%")
(format t "いいだ~%")
(format t "いしだ~%")
(format t "はらだ~%")
(format t "~%")

(defparameter *hiragana* "いぃろはばぱにほぼぼへべぺとどちぢりぬるをわかがよただれそぞつづっねならむうぅのおぉくぐやまけげふぶぷこごえぇてであぁさざきぎゆめみしじひびぴもせぜすず")
(defparameter *katakana* "イィロハバパニホボボヘベペトドチヂリヌルヲワカガヨタダレソゾツヅッネナラムウゥノオォクグヤマケゲフブプコゴエェテデアァサザキギユメミシジヒビピモセゼスズ")

(defun iroha (arg)
  (setq arg (convert-string arg nil nil))
  (setq arg (selection-sort arg (list *hiragana* *katakana*) nil))
  (show-string arg))

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

(defun show-string (arg)
  (cond
    ((equal arg nil)
     (format t "~%~%")
     (return-from show-string nil))
    (t
      (format t "~%")
      (format t (car arg))
      (show-string (cdr arg)))))

(defun selection-sort (target order-list result)
  (push (select-first target order-list) result)
  (setq target (remove (select-first target order-list) target :count 1))
  (if (equal target nil)
      (return-from selection-sort (reverse result))
      (selection-sort target order-list result)))

(defun select-first (target order-list)
  (let ((winner (car target)))
    (dotimes (i (length target) nil)
      (setq winner (car (comparison winner (nth i target) order-list))))
    (return-from select-first winner)))

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
