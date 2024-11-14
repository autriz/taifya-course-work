begin
    var a, b, c, d: %;
        e: !;;
    a := 5o + 10 * 12h / 0101b;
    for c := 1 to 10 step 2 writeln "got ", c next;
    while (c < 20) c := c + 10o;
    writeln "after while: ", c;
    readln d;
    if ((d == 42) || (c == 20)) 
        b := 10h
    else begin 
        b := 10o; 
        e := 12.34;
        writeln "hi" 
    end;
    writeln b
end