let util = import "./util.fp";

let log = util.log;
let alert = util.alert;
let div = util.div;
let span = util.span;
let input = util.input;
let button = util.button;
let iframe = util.iframe;
let select = util.select;
let option = util.option;

let search x = {
    [
        (string.concat "https://www.google.com/webhp?igu=1&q=" x),"c1";
        (string.concat "https://cn.bing.com/search?&q=" x),"c2";
        (string.concat "https://www.baidu.com/s?wd=" x),"c3"
    ]
        |> list.each fn x,y = {
              y |> documentHelper.getElementById |> documentHelper.setAttr "src",x;
          }
        |> ignore;
}

let onclick () = {
    "searchinput"
        |> documentHelper.getElementById
        |> documentHelper.getProp "value"
        |> search
        |> ignore;
}

let getQuery () = {
    if window.location.search {
        window.location.search  |> string.replace "?q=","";
    } else {
        "";
    };
}

let initDom () = {
    let parent =
        div {} [
          div {
                  "style","position:fixed;width:100%;box-shadow:0 2px 8px #f0f1f2;padding-bottom:1rem;padding-top:1rem;display: flex;justify-content: center;align-items: center;position: fixed;width: 100%;"
              } [
                  input {
                      "id","searchinput";
                      "value", if window.location.search {getQuery();} else {"";}
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

    documentHelper.append documentHelper.body parent |> ignore;
}

let initEvent () = {
    document.addEventListener "keyup",fn x = {
        if x.code = "Enter" {
            onclick();
        };
        ();
    };
  "selecter"
      |> documentHelper.getElementById
      |> documentHelper.addListener "change",fn x = {
          {
            "google",("c1", ["c2";"c3"]);
            "bing",("c2", ["c1";"c3"]);
            "baidu",("c3", ["c2";"c1"])
          }.(x.target.value)
          |> fn v1,v2 = {
                v1
                |> documentHelper.getElementById
                |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:block";
                v2
                |> list.each fn x = {
                   x
                   |> documentHelper.getElementById
                   |> documentHelper.setAttr "style","border:none;width:100%;height:85vh;display:none";
                };
           };
        }
       |> ignore;
}

let autoSearch () = {
  if window.location.search {
    search (getQuery());
  } |> ignore;
}

let start = initDom + initEvent + autoSearch;

start() |> ignore;
