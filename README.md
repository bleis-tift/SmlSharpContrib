# SML# contrib
[![wercker status](https://app.wercker.com/status/8577ef8c41de69d1457c588dd15d5229/m "wercker status")](https://app.wercker.com/project/bykey/8577ef8c41de69d1457c588dd15d5229)

## 概要
TBD

## 実装対象
[Issue #1](https://github.com/bleis-tift/SmlSharpContrib/issues/1)

## 方針

 * SML/NJ等のライブラリをひっぱてくるときはライセンスの整合性を確認すること。(要BSD系ライセンスとの互換性)
 * モジュールは階層構造をせずにフラットにする
 * [1モジュール1データ型主義](http://d.hatena.ne.jp/camlspotter/20121216/1355686499)を採用する

## ビルド方法
### exampleのビルド
`make all` で `example/` 以下のファイルをビルドできます。

```shell
$ make depend
$ make all
```

実行:

```shell
$ ./example/hello
```

### テストの実行

```shell
$ make check
./testRunner
.
tests = 1, failures = 0, errors = 0
Failures:
Errors:
```
