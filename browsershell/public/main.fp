
let a = [1;2;3;4;5];

let showMessage2 x = {
    fn m = {
        alert (m + x);
    };
}

let p = "F+!" |> showMessage2;
"hello " |> p;
();