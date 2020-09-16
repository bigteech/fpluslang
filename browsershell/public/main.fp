let search x = {
    let l1 = string.concat "https://www.google.com/webhp?igu=1&q=" x;
    let l2 = string.concat "https://cn.bing.com/search?&q=" x;
    let l3 = string.concat "https://www.baidu.com/s?wd=" x;
    "c1"
        |> documentHelper.getElementById
        |> documentHelper.setAttr ("src",l1);
    "c2"
        |> documentHelper.getElementById
        |> documentHelper.setAttr ("src",l2);
    "c3"
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
let select = documentHelper.createElement "select";
let option = documentHelper.createElement "option";

if window.location.search {
  let newQ = window.location.search  |> string.replace ("?q=", "");
  ();
};

let parent =
    div {} [
      div {
              "style","position:fixed;width:100%;box-shadow:0 2px 8px #f0f1f2;padding-bottom:1rem;padding-top:1rem;display: flex;justify-content: center;align-items: center;position: fixed;width: 100%;"
          } [
              select {"id", "selecter"; "style", "
                  height: 36px;
                  border-radius: 6px;
                  border: 1px solid #c5c1c1;
                  margin-right: 5px;
              "} [
                option {"value", "google"} ["google"];
                option {"value","bing"} ["bing"];
                option {"value", "baidu"} ["百度"]
              ];
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
      div {"style","display:inline-block;width:100%;margin-top:5rem"} [
        iframe {"id","c1";"style","border:none;width:100%;height:90vh"} []
      ];
      div {"style","display:inline-block;width:100%;margin-top:5rem"} [
        iframe {"id","c2";"style","border:none;width:100%;height:90vh;display:none"} []
      ];
      div {"style","display:inline-block;width:100%;margion-top:5rem"} [
        iframe {"id","c3";"style","border:none;width:100%;height:80vh;display:none"} []
      ]
    ];

document.addEventListener "keyup",(fn x = {
    if (x.code = "Enter") {
        onclick();
    };
    ();
});

documentHelper.append documentHelper.body parent;

"selecter"
    |> documentHelper.getElementById
    |> documentHelper.addListener "change",fn x = {
         if x.target.value = "google" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:block");
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");

         };
         if x.target.value = "bing" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:block");
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");
        };
        if x.target.value = "baidu" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:none");
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr ("style","border:none;width:100%;height:85vh;display:block");
        };
       };



if window.location.search {
  search newQ;
  ();
};
