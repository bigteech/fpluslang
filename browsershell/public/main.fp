let log,alert,div,span,input,button,iframe,select,option,toView,domRender = import "./util.fp";

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

let getQuery () = {
    if window.location.search {
        window.location.search  |> string.replace "?q=","";
    } else {
        "";
    };
}

let TopBox = toView fn props,state,helper = {
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
            ";
            "onchange", fn x = {
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
            } (props.options |> list.map fn x,y = {option {"value",x.0} [x.1];})
        ];
};

let Main = toView fn props,state,helper = {
  div {} [
    TopBox {"options",[("google","谷歌");("bing","Bing");("baidu","百度")]} [];
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
};

let initDom () = {
  domRender (documentHelper.body,(Main {} []));
}

let initEvent () = {
    document.addEventListener "keyup",fn x = {
        if x.code = "Enter" {
            "searchinput"
                |> documentHelper.getElementById
                |> documentHelper.getProp "value"
                |> search
                |> ignore;
        } |> ignore;
    };
}

let tryAutoSearch () = {
  if window.location.search {
    getQuery() |> search;
  } |> ignore;
}

let start = initDom + initEvent + tryAutoSearch;

start() |> ignore;
