let log = window.console.log;
let alert = window.alert;

let view domGen = {
   fn () = {
     let state = {};
     let helper = {"setState", fn x = {
         dict.effectUpdate state,x;
         documentHelper.replaceWith state.___previous__,(domGen state.props,state,helper);
     }};
     fn props = {
        let ret = domGen (props, state, helper);
        dict.effectUpdate state,{"___previous__",ret; "props",props};
        ret;
      };
   };
};

{
  [
    "div";
    "span";
    "input";
    "button";
    "iframe";
    "select";
    "option"
  ] |> list.map fn x = {
          x,(documentHelper.createElement x);
      }
    + ["log",log;"alert",alert;"toView",view]
};
