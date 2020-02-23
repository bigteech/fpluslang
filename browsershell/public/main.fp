
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
        Input {"id","searchInput";"style","padding-left:0.5rem;outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"} [];
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