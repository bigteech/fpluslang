let fib x = {
  if (x <= 0) {

  0;} else {
  x + fib (x - 1);
  };
}
fib 4 |> print;


let a = [1;2;3;4;5] + [7];
a |> list.find fn x = { x = 7; } |> print;
a |> list.findIndex fn x = { x = 7; } |> print;
let b = {"1",2} + {"2", 3};
(b.("2") + b.("1")) |> print;
();