
let onclick () = {
    "searchInput"
        |> documentHelper.getElementById
        |> documentHelper.getProp "value"
        |> string.concat "https://www.baidu.com/s?wd="
        |> windowHelper.goto;
}

let log = window.console.log;   
let alert = window.alert; 
let Div = documentHelper.createElement "div";
let Span = documentHelper.createElement "span";
let Input = documentHelper.createElement "input";
let Button = documentHelper.createElement "button";
let parent = 
    Div {
            "style","margin-top:10rem;display: flex;justify-content: center;align-items: center;"
        } [
            Input {
                "id","searchInput";
                "style","padding-left:0.5rem;outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"
            } [];
            Button {
                "style","outline:none;height: 2rem;margin-left: 1rem;border-radius: 0.2rem;border:none";
                "onclick",onclick
            } ["搜索"]
        ];

document.addEventListener "keyup",(fn x = {
    if (x.code = "Enter") {
        onclick();
    };
    ();
});

documentHelper.append documentHelper.body parent;


let fib x = {
  if (x <= 0) {
    0;
  } else {
  x + fib (x - 1);
  };
}
fib 4 |> log;


let a = [1;2;4;5] + [7];
a |> list.findIndex fn x = { x = 7; } |> log;
let b = {"1",2} + {"2", 3};
(b.("2") + b.("1")) |> log;
(b.2 + b.1) |> log;
(b.(1+1) + b.("1")) |> log;
();