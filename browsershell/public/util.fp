let log = window.console.log;
let alert = window.alert;

let rawTag = [
    "div";
    "span";
    "input";
    "button";
    "iframe";
    "select";
    "option"
];
let rawComponent = rawTag |> list.map fn x = {
  x,(documentHelper.createElement x);
};

let createElement component = {
  fn props = {
    fn children = {
      {
        "component", component;
        "props", props;
        "children", children
      };
    };
  };
}
let unstruct param = {
  param.component param.props (param.children |> list.map fn x = {
    if (typeof x) = "string" {
      x;
    } else {
      unstruct x;
    };
  });
}

let domRender dom,com = {
  let component,props,children = com;
  let publicRender,privateState = component();
  let ret = unstruct(publicRender (props,children));
  documentHelper.append dom ret;
  dict.effectUpdate privateState,{"dom",ret};
}

let view domGen = {
   let ret () = {
     let state = {};
     let privateState = {"firstRender", true; "renderMap", {}; "props",{}; "dom",()};
     let helper = {"setState", fn x = {
         dict.effectUpdate state,x;
         let ret = unstruct (render());
         if privateState.dom {
          documentHelper.replaceWith privateState.dom,ret;
        };
         dict.effectUpdate privateState,{"dom",ret};
     }};
     let publicRender props,children = {
        dict.effectUpdate privateState,{"props",props};
        dict.effectUpdate privateState.props,{"children",children};
        render();
     }
     let render () = {
         let renderMap = privateState.renderMap;
         let pri = domGen privateState.props,state,helper;
         let diffAndUpdate x,y,z = {
            let isRawComponent = rawComponent |> list.exists fn x = { x.1 = z.component;};
            if isRawComponent {
                z;
            } else {
               let cache = renderMap.(string.from x + string.from y);
               if cache.component = z.component  {
                  let realChildren = z.children |> list.map (fn child,idx = {
                     if (typeof child) = "string" {
                       child;
                     } else {
                       diffAndUpdate x,idx,child;
                     };
                  });
                  let ret = cache.render (z.props,realChildren);
                  ret;
               } else {
                  let pubRender,privateState = z.component();
                  let realChildren = z.children |> list.map (fn child,idx = {
                     if (typeof child) = "string" {
                       child;
                     } else {
                       diffAndUpdate x,idx,child;
                     };
                  });
                  let ret = pubRender (z.props),(realChildren);
                  dict.effectUpdate renderMap,{(string.from x + string.from y),{"component", (z.component);"render", pubRender}};
                  ret;
               };
            };
         };
         diffAndUpdate 0,0,pri;
     }
     publicRender,privateState;
   };
   createElement ret;
};

{
  (rawComponent |> list.map fn obj,index = {
    let x,y = obj;
    x,(view (fn props = {
      createElement y props props.children;
    }));
  }) + ["log",log;"alert",alert;"toView",view; "domRender",domRender]
};
