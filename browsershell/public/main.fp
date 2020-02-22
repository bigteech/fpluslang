
let a = [1;2;3;4;5];

let showMessage2 x = {
    fn m = {
        alert (m + x);
    };
}

let p = "F+!" |> showMessage2;

let onclick x = {
    "hello " |> p;
}
    
let Div = document.createElement "div";
let child = Div {"onclick",onclick;"class","hhhh";"style","height:11rem;width:12rem;background-color: blue"} ["fwsfds"];
let parent = Div {"class","hhhh";"style","height:12rem;width:13rem;background-color: red"} [child];

document.append document.body parent;
();