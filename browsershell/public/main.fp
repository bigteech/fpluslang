let log = window.console.log;
let alert = window.alert;
let div = documentHelper.createElement "div";
let span = documentHelper.createElement "span";
let input = documentHelper.createElement "input";
let button = documentHelper.createElement "button";
let iframe = documentHelper.createElement "iframe";
let select = documentHelper.createElement "select";
let option = documentHelper.createElement "option";

let search x = {
    let l1 = string.concat "https://www.google.com/webhp?igu=1&q=" x;
    let l2 = string.concat "https://cn.bing.com/search?&q=" x;
    let l3 = string.concat "https://www.baidu.com/s?wd=" x;
    "c1"
        |> documentHelper.getElementById
        |> documentHelper.setAttr "src",l1;
    "c2"
        |> documentHelper.getElementById
        |> documentHelper.setAttr "src",l2;
    "c3"
        |> documentHelper.getElementById
        |> documentHelper.setAttr "src",l3;
    ();
}

let onclick () = {
    "searchinput"
        |> documentHelper.getElementById
        |> documentHelper.getProp "value"
        |> search;
}

if window.location.search {
  let newQ = window.location.search  |> string.replace "?q=","";
  ();
};

let parent =
    div {} [
      div {
              "style","position:fixed;width:100%;box-shadow:0 2px 8px #f0f1f2;padding-bottom:1rem;padding-top:1rem;display: flex;justify-content: center;align-items: center;position: fixed;width: 100%;"
          } [
              input {
                  "id","searchinput";
                  "value", if window.location.search {newQ;} else {"";}
              } [];
              select {"id", "selecter"; "style", "
                  height: 36px;
                  margin-right: 5px;
                  margin-left: 10px;
              "} [
                option {"value", "google"} ["google"];
                option {"value","bing"} ["bing"];
                option {"value", "baidu"} ["百度"]
              ]
          ];
      div {"style","display:inline-block;width:100%;margin-top:5rem"} [
        iframe {"id","c1";"style","border:none;width:100%;height:85vh"} []
      ];
      div {"style","display:inline-block;width:100%;"} [
        iframe {"id","c2";"style","border:none;width:100%;height:85vh;display:none"} []
      ];
      div {"style","display:inline-block;width:100%;"} [
        iframe {"id","c3";"style","border:none;width:100%;height:85vh;display:none"} []
      ]
    ];

document.addEventListener "keyup",fn x = {
    if x.code = "Enter" {
        onclick();
    };
    ();
};

documentHelper.append documentHelper.body parent;

"selecter"
    |> documentHelper.getElementById
    |> documentHelper.addListener "change",fn x = {
         if x.target.value = "google" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:block";
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";

         };
         if x.target.value = "bing" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:block";
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
        };
        if x.target.value = "baidu" {
            "c1"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
            "c2"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
            "c3"
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:block";
        };
       };

if window.location.search {
  search newQ;
  ();
};

