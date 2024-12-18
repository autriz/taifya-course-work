(* Начало программы *)
begin
    (* Объявление переменных *)
    var a, b, c, d, f: %;
        e: !;;

    var;

    (* Использование восьмеричного числа *)
    a := 5o + 10 * 12h - 0101b;

    (* Цикл for *)
    for c := 1 to 10 step 2 writeln "got ", c next;

    (* Цикл while *)
    while (c < 20) c := c + 10o;

    (* Вывод в консоль *)
    writeln "after while: ", c;

    (* Ввод данных из консоли *)
    readln d;

    (* writeln "got ", d; *)

    (* Условная операция *)
    if (d == 42) 
        b := 10h
    (* Вложенный блок *)
    else begin 
        b := 10o; 
        e := 12.34;

        writeln "hi" 
    end;

    writeln b
end
