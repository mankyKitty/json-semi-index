# JSON Semi-Index Implementation

A few implementations of the 'semi-indexer' described in [this paper](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf).

Run the `dev.sh` to use `ghcid` to mess around with the implementation and see
how it affects the index. The primary purpose of this was to be able to retrieve
the closing character of a Array or Object to confirm that it was closed with
the correct character.

```
Input     {"a": 1, "b": {"l": [1, null], "v": true}}
Length    42
Positions 100010010000101000101010000011000010000011
BP        (()()()((()((()()))()())))
```

```
BitVectors!
Input    {"a": 1, "b": {"l": [1, null], "v": true}}
Length   42
Pos      10001001 00001010 00101010 00001100 00100000 11000000 00000000 00000000
BP Upper 10001010 00000000 00000000 00000000 00000000 00000000 00000000 00000000
BP Lower 11111111 01100000 00000000 00000000 00000000 00000000 00000000 00000000
"Close Rank @  Just 12"
"Close Char @  Just 41"
"Close Char =  Just '}'"

BitVectors!
Input    {"foo":{"a":3},"bar":[5]}
Length   25
Pos      10000011 00010110 00001101 10000000 00000000 00000000 00000000 00000000
BP Upper 10100001 00000000 00000000 00000000 00000000 00000000 00000000 00000000
BP Lower 11110111 00000000 00000000 00000000 00000000 00000000 00000000 00000000
"Close Rank @  Just 9"
"Close Char @  Just 24"
"Close Char =  Just '}'"

BitVectors!
Input    [[1],[5]]
Length   9
Pos      11011101 10000000 00000000 00000000 00000000 00000000 00000000 00000000
BP Upper 11001000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
BP Lower 11011000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
"Close Rank @  Just 6"
"Close Char @  Just 8"
"Close Char =  Just ']'"
```
