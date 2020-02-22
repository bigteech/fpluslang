
let onclick x = {
    let p = "hello world";
    fn m = {
        alert (p + x);
    };
}
    
let Div = document.createElement "div";
let Span = document.createElement "span";
let parent = 
    Div {"class","hhhh";"style","height:12rem;width:13rem;background-color: red"} [
        Div {"onclick",(onclick "!");"class","hhhh";"style","height:11rem;width:12rem;background-color: blue"} [
            Span {} [
                "hello"
            ];
            Span {} [
                "world"
            ]
        ]
    ];

document.append document.body parent;

();