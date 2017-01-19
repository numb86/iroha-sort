(defparameter *hiragana* "あいうえお")
(defparameter *katakana* "アイウエオ")
(format t "~%")

(defun test (title result expect)
	(cond
   ((not (equal result expect))
    (print title)
    (format t "~%"))))

(test "convert-string-1" (convert-string '(いえ　ううお　あい　あえ) nil nil) (list "あえ" "あい" "ううお" "いえ"))
(test "convert-string-2" (convert-string '(いえ A B　CD あい　あえ) nil nil) (list "あえ" "あい" "CD" "B" "A" "いえ"))
(test "convert-string-3" (convert-string '(いえ ) nil nil) (list "いえ"))

(test "selection-sort-1" (selection-sort (list "イ" "アア" "おあ" "あオ") (list *hiragana* *katakana*) nil) (list "アア" "あオ" "イ" "おあ"))
(test "selection-sort-2" (selection-sort (list "あ" "あAい" "あA" "Aお" "え") (list *hiragana* *katakana*) nil) (list "あ" "あA" "あAい" "え" "Aお"))

(test "select-first-1" (select-first (list "あいう" "あいえ" "あいあ") (list *hiragana* *katakana*)) "あいあ")
(test "select-first-2" (select-first (list "あい") (list *hiragana* *katakana*)) "あい")

(test "comparison-1" (comparison "アルファベット" "いろは" (list *hiragana* *katakana*)) (list "アルファベット" "いろは"))
(test "comparison-2" (comparison "ルアル" "ルアい" (list *hiragana* *katakana*)) (list "ルアい" "ルアル"))

(test "search-place-1" (search-place "あいうえお" 0 (list *hiragana* *katakana*)) 0)
(test "search-place-1" (search-place "えおあいう" 0 (list *hiragana* *katakana*)) 3)
(test "search-place-1" (search-place "えおあいう" 4 (list *hiragana* *katakana*)) 2)

(defparameter *hiragana* "いぃろはばぱにほぼぼへべぺとどちぢりぬるをわかがよただれそぞつづっねならむうぅのおぉくぐやまけげふぶぷこごえぇてであぁさざきぎゆめみしじひびぴもせぜすず")
(defparameter *katakana* "イィロハバパニホボボヘベペトドチヂリヌルヲワカガヨタダレソゾツヅッネナラムウゥノオォクグヤマケゲフブプコゴエェテデアァサザキギユメミシジヒビピモセゼスズ")

(format t "~%")
