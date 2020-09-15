let search x = {
    let l2 = string.concat "https://www.google.com.hk/search?q=" x;
    let l3 = string.concat "https://cn.bing.com/search?&q=" x;
    windowHelper.goto l3;
    windowHelper.goto l2;
    ();
}

if window.location.search {
  let newQ = window.location.search  |> string.replace ("?q=", "");
  search newQ;
  ();
};

let log = window.console.log;
let alert = window.alert;
let onclick () = {
    "searchinput"
        |> documentHelper.getElementById
        |> documentHelper.getProp "value"
        |> search;
}

let div = documentHelper.createElement "div";
let span = documentHelper.createElement "span";
let input = documentHelper.createElement "input";
let button = documentHelper.createElement "button";
let parent =
    div {
            "style","margin-top:10rem;display: flex;justify-content: center;align-items: center;"
        } [
            input {
                "id","searchinput";
                "style","padding-left:0.5rem;outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"
            } [];
            button {
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
