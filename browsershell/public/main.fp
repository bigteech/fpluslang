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

// let comment = toView (fn props,state,helper = {
//     let a = if state.a {state.a;} else {"www";};
//     button {
//       "onclick", fn x = {helper.setState {"a", "1"};};
//       "id","hello"
//    } [a];
// });

let Main = toView (fn props,state,helper = {
  let t = if state.m {state.m;} else {"sssssssss";};
  div {"style","stylem"; "onclick", fn () = {helper.setState {"m", "11111111"};}} [
    t
  ];
});

let initDom () = {
  domRender (documentHelper.body,(Main {} []));
}

let initEvent () = {
    document.addEventListener "keyup",fn x = {
        if x.code = "Enter" {
            onclick();
        } |> ignore;
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
    getQuery() |> search;
  } |> ignore;
}

let start = initDom;

start() |> ignore;
