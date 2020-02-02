
let a = [1;2;3;4;5];

a |> list.find fn x = { x = 3; } |> print;
a |> list.findIndex fn x = { x = 3; } |> print;
();