# mozc-temp

[![Build Status](https://travis-ci.org/HKey/mozc-temp.svg?branch=master)](https://travis-ci.org/HKey/mozc-temp)
[![MELPA](https://melpa.org/packages/mozc-temp-badge.svg)](https://melpa.org/#/mozc-temp)
[![MELPA Stable](https://stable.melpa.org/packages/mozc-temp-badge.svg)](https://stable.melpa.org/#/mozc-temp)

mozc-tempはmozc.elによる入力をモードレス化するラッパーです。

[ac-mozc](https://github.com/igjit/ac-mozc)をもとに作成されました。

基本的な挙動はac-mozcと同じになるように作られていて、全角文字と半角文字の混在する文章の入力を楽にすることを目的としています。

ac-mozcとの違いは、これがmozc.elのインターフェイスに対するラッパーであるということです。
そのため、変換時の候補選択はmozc.elのものと同じ操作が可能です。

## 使い方

変換したい文字列（ローマ字など）を入力した後、`mozc-temp-convert`、もしくは`mozc-temp-convert-dwim`を呼び出します。
すると、mozc.elの入力モードが起動するので、普段通り変換操作をします。
変換を確定するか変換中の文字列を全て削除すると、通常の入力状態に戻ります。

コマンドは呼び出しやすくするためにキーに割り当てておくといいでしょう。

```emacs-lisp
(global-set-key (kbd "M-n") #'mozc-temp-convert)
```

## インストール

mozc-tempは[MELPA](https://melpa.org/#/)、もしくは[MELPA Stable](https://stable.melpa.org/#/)から下記の手順でインストールすることができます。

1. MELPA、もしくはMELPA Stableを使うよう、Emacsのパッケージマネージャーを設定します。  
   設定方法に関するMELPAのドキュメントは[ここ](https://github.com/melpa/melpa#usage)にあります。

2. mozc-tempをインストールします。  
   `M-x package-install mozc-temp`

## バージョニング

mozc-tempのバージョニングは[Semantic Versioning 2.0.0](http://semver.org/spec/v2.0.0.html)に従います。

## コマンド

mozc-tempが提供するコマンドは下記の2つです。

### `mozc-temp-convert`

このコマンドはカーソルの直前にある未変換の文字列を変換します。

変換を確定するか、変換中の文字列を全て削除するとこのコマンドは終了し、通常の入力状態に戻ります。

### `mozc-temp-convert-dwim`

このコマンドは`mozc-temp-convert`のDWIM版です。

このコマンドを呼び出した時、カーソルの直前に変換可能な文字列があれば`mozc-temp-convert`を呼び出します。
そうでない場合は、そのままmozc.elによる入力状態に移ります。
この時も変換を確定するか、変換中の文字列を全て削除すると、通常の入力状態に戻ります。
