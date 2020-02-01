
let a = [1;2;3;4;5];

let k = a |> list.map  fn x = { x + 2; };
k |> list.each fn x = { print x ;};
k |> list.length |> print;
();