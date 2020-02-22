
let onclick x = {
    let p = "hello world";
    fn m = {
        alert (p + x);
    };
}
    
let Div = document.createElement "div";
let parent = 
    Div {"class","hhhh";"style","height:12rem;width:13rem;background-color: red"} [
        Div {"class","hhhh";"style","height:11rem;width:12rem;background-color: blue"} [
            Div {"onclick",(onclick "!");"class","hhhh";"style","height:8rem;width:5rem;background-color: yellow"} [
                
            ];
            Div {"onclick",(onclick "!");"class","hhhh";"style","height:4rem;width:2rem;background-color: yellow"} [
                "hello"
            ]
        ]
    ];

document.append document.body parent;

();