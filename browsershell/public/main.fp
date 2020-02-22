
let onclick x = {
    let p = "hello world";
    fn m = {
        alert (p + x);
    };
}
    
let Div = document.createElement "div";
let child = Div {"onclick",(onclick "!");"class","hhhh";"style","height:11rem;width:12rem;background-color: blue"} ["fwsfds"];
let parent = Div {"class","hhhh";"style","height:12rem;width:13rem;background-color: red"} [child];

document.append document.body parent;
();