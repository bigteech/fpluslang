
let a = [1;2;3;4;5];

let openMessageBox x = {
    alert x;
}

"hello  F+!" |> openMessageBox;
a |> list.find fn x = { x = 3; } |> openMessageBox;
a |> list.findIndex fn x = { x = 3; } |> openMessageBox;
();