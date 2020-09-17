let log = window.console.log;
let alert = window.alert;

let exports = ([
    "div";
    "span";
    "input";
    "button";
    "iframe";
    "select";
    "option"
] |> list.map fn x = {
    x,(documentHelper.createElement x);
} + ["log",log;"alert",alert]) |> dict.create;

exports;
