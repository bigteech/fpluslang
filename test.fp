
let a = [1;2;3;4;5];

let k = a |> list.map  fn x = { x + 2; };

print k.[0];
