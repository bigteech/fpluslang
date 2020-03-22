
let a = [1;2;3;4;5] + [7];

a |> list.find fn x = { x = 7; } |> print;
a |> list.findIndex fn x = { x = 7; } |> print;
();