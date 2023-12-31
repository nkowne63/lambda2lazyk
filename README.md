# lambda2lazyk

## Lazy Kとは何か

[Lazy K](https://tromp.github.io/cl/lazy-k.html)はきわめてシンプルな関数型言語です。
具体的には、Lazy Kのプログラムは以下の3つの関数の組み合わせだけで書かれます。

- S: \xyz.xz(yz)
- K: \xy.x
- I: \x.x

他の関数型言語と比べると、以下のような特徴があります。

### 型システムが存在しない

SKIしか存在しないので型推論や型システムが存在しません。
自然数を表現したいときはChurch encodingという手法を使う必要があります。

### 入出力が非決定的

[公式サイト](https://tromp.github.io/cl/lazy-k.html)によると、Lazy Kには正確な入出力同期メカニズムを持っていないようです。なので、入出力が関わるプログラムを書くと順番が入れ替わるみたいなことが起こりうるようです。
（これが言語の操作的意味論に起因する問題なのか、処理系の実装に依存する問題なのかは把握できていません。Haskellのモナドのようなものを再実装すると問題ないような気がしますが...）

## このリポジトリは何か

このリポジトリは[T[]-変換](https://zenn.dev/zk_phi/books/5f0430586c25dc0a70ae/viewer/eba7bc81fe286456ceb7)に従ってlambda termをLazy KのSKIの組み合わせに変換するプログラムと、そのプログラムでYコンビネータをSKI表現に変換する例を挙げています。

本当はLazy Kの処理系そのものを作ってHello Worldしたかったのですがだいぶ最適化が辛そうだったので雰囲気だけ味わうことにしました。

## 署名（本人確認用）
Rei Tokami（冬鏡 澪）
