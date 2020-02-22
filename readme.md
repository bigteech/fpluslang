# F+ 
顾形思义，F#的精简版，是本人在武汉肺炎的时候无聊搞的一种船新的语言，全名Fpluslang。
语法糖简单优美深得我心。
```f#
// 函数声明
let createdict k,v,m,n = {
    // 字典
    {k,v;m,n};
}

// 函数调用
let mydict = createdict "a","2","b","4";

// 管道调用
mydict.a |> print;

// lambda表达式与管道
mydict.b,mydict.a
    |> fn x,y = { x + " i am lambda exp " + y; }
    |> print;

let dict2 = {"m",1;"n",2};
dict2.m |> print;
dict2.n |> print;

let readfile path = {
    let p = ["read", "read2"];
    path |> file.[p.["0"]] |> print;
    p.["1"] |> print;
    path |> file.[p.["0"]];
}

readfile "./test.txt" |> print;

let v = readfile "./test.txt";

if v {
   print v;
} else {
   print 2,(4+1),4;
};


let hf text = {
    let m = if 0 {
        "1";
    } else {
        "2";
    };
    m;
}

let hf2 x = {
    fn k = {
        k + "hf2";
    };
}


// 连续调用

hf2 hf "test hf" |> print;


// 没参数的函数
let hf3 () = {
    print "
        空方法
    ";
}

hf3 ();

// tuple 元组参数自动解构

print 1,2,(1,2),1;
```
## 浏览器上运行
```f#

let onclick x = {
    "searchInput"
        |> documentHelper.getElementById
        |> documentHelper.getProp "value"
        |> string.concat "https://www.baidu.com/s?wd="
        |> windowHelper.goto;
}
    
let Div = documentHelper.createElement "div";
let Span = documentHelper.createElement "span";
let Input = documentHelper.createElement "input";
let Button = documentHelper.createElement "button";
let parent = 
    Div {"style","margin-top:10rem;display: flex;justify-content: center;align-items: center;"} [
        Input {"id","searchInput";"style","outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"} [];
        Button {"style","outline:none;height: 2rem;margin-left: 1rem;border-radius: 0.2rem;";"onclick",onclick} ["搜索"]
    ];

document.addEventListener "keyup",(fn x = {
    if (x.code = "Enter") {
        "searchInput"
            |> documentHelper.getElementById
            |> documentHelper.getProp "value"
            |> string.concat "https://www.baidu.com/s?wd="
            |> windowHelper.goto;
    };
    ();
});
documentHelper.append documentHelper.body parent;
```
<img src="./browser.png">