# assembly
assembly

# build
* たぶんocamlbuildがあれば大丈夫?
* ppx_derivingとかも必要かも

```
make
```
でassembleが出るはず

# usage
```
./assemble test.s
```
でtest.s.ooにバイナリがはかれる.

1st向けは
```
./assemble test.s -32bit
```
2nd向けにテキストを吐くには
```
./assemble test.s  -txt
```
2nd向けにバイナリを吐くには
```
./assemble test.s 
```
