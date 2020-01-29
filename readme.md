# minifsharp
顾名思义，小的F#，是本人在武汉肺炎的时候无聊搞的一种船新的语言，全名minifsharplang。
语法糖优美简单深得我心。
```f#
let readfile path = {
    let p = ["read", "read2"];
    file.[p.["0"]] path;
}

let v = readfile "./test.txt";

if v {
   print v;
} else {
   print 2,(4+1),4;
}
```