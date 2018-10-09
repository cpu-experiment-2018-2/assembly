# assembly
assembly

# build
* たぶんocamlbuildがあれば大丈夫?
* ppx_derivingとかも必要かも

```
make
```
でmain.nativeが出るはず

# usage
```
./main.native test.s
```
でtest.s.ooにバイナリがはかれる.

```
./main.native test.s -txt
```
でtest.s.txtに文字列の２進数表記がでる

* 文法はtest.sに書いてあるのがほとんど

