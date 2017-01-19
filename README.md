```Common Lisp
```の練習のために作成。

与えられた文字列をいろは順にソートして表示する。平仮名と片仮名のみ対応。

処理系は```CLISP```で動作確認している。


# 使い方

対話環境にて、```iroha.lisp```を読み込む。

```lisp
(load "iroha.lisp")
```

以下の形で文字列を渡すと、ソートした結果が表示される。  
全角、もしくは半角スペースが、単語の区切りと認識される。  
平仮名と片仮名の混在も可能。

```lisp
(iroha '(単語　単語))
```

## 例

```lisp
(iroha '(いしだ　はらだ　いいだ))
```

```
いいだ
いしだ
はらだ
```

```lisp
(iroha '(
いいの
はしもと
ホシ
はしの
いしかわ
ほしの
いイだ
))
```

```
いイだ
いいの
いしかわ
はしの
はしもと
ホシ
ほしの
```

```lisp
(iroha '(へ ろ に い は ほ ろ い と))
```

```
い
い
ろ
ろ
は
に
ほ
へ
と
```

***

ソートの方法には、最も簡単な単純選択法を用いている。  
もしかしたら今後、```Common Lisp```やアルゴリズムの練習として、クイックソートなどの他の方法も作るかもしれない。
