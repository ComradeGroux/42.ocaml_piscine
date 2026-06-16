let rec repeat_string ?str:(s="x") n = 
	if n < 0 then
		"Error"
	else if n == 0 then
		""
	else
		s ^ repeat_string ~str:s (n - 1)


let () =
	print_endline ("repeat \"bla\" 3 times : \"" ^ (repeat_string ~str:"bla" 3) ^ "\"");
	print_endline ("repeat 3 times : \"" ^ (repeat_string 3) ^ "\"");
	print_endline ("repeat -3 times : \"" ^ (repeat_string (-3)) ^ "\"");
	print_endline ("repeat \"cou\" -3 times : \"" ^ (repeat_string ~str:"cou" (-3)) ^ "\"")
