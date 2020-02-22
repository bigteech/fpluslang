
let onclick x = {
    "searchInput"
        |> document.getElementById
        |> document.getProp "value"
        |> string.concat "https://www.baidu.com/s?wd="
        |> window.goto;
}
    
let Div = document.createElement "div";
let Span = document.createElement "span";
let Input = document.createElement "input";
let Button = document.createElement "button";
let parent = 
    Div {"style","display: flex;justify-content: center;align-items: center;"} [
        Input {"id","searchInput";"style","outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"} [];
        Button {"style","outline:none;height: 2rem;margin-left: 1rem;border-radius: 0.2rem;";"onclick",onclick} ["搜索"]
    ];

document.append document.body parent;