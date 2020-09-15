let search x = {
    let l1 = string.concat "https://www.baidu.com/s?wd=" x;
    let l2 = string.concat "https://www.google.com/custom?btnG=Search&q=" x;
    let l3 = string.concat "https://cn.bing.com/search?&q=" x;
    l2 |> windowHelper.goto;
    "c1"
        |> documentHelper.getElementById
        |> documentHelper.setAttr ("src",l1);
    "c2"
        |> documentHelper.getElementById
        |> documentHelper.setAttr ("src",l3);
    ();
}

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
let iframe = documentHelper.createElement "iframe";
if window.location.search {
  let newQ = window.location.search  |> string.replace ("?q=", "");
  ();
};
let parent =
    div {} [
      div {
              "style","margin-top:1rem;display: flex;justify-content: center;align-items: center;"
          } [
              input {
                  "id","searchinput";
                  "value", if window.location.search {newQ;} else {"";};
                  "style","padding-left:0.5rem;outline:none;font-size:larger;width: 35rem;height: 2rem;border-radius: 0.3rem;border: 1px solid #c5c1c1;"
              } [];
              button {
                  "style","outline:none;height: 2rem;margin-left: 1rem;border-radius: 0.2rem;border:none";
                  "onclick",onclick
              } ["搜索"]
          ];
      div {"style","display:inline-block;width:50%"} [
        iframe {"id","c2";"style","border:none;width:100%;height:1000px"} []
      ];
      div {"style","display:inline-block;width:50%"} [
        iframe {"id","c1";"style","border:none;width:100%;height:1000px"} []
      ]
    ];

document.addEventListener "keyup",(fn x = {
    if (x.code = "Enter") {
        onclick();
    };
    ();
});

documentHelper.append documentHelper.body parent;

if window.location.search {
  search newQ;
  ();
};
