let repeat_x n = 
	if n < 0 then
		"Error"
	else
		let rec repeat x = 
			if x <= 0 then
				""
			else
				"x" ^ repeat (x - 1)
		in "" ^ (repeat n)

let () = 
	print_endline ("-1: \"" ^ (repeat_x (-1)) ^ "\"");
	print_endline ("3: \"" ^ (repeat_x 3) ^ "\"");
	print_endline ("0: \"" ^ (repeat_x 0) ^ "\"");
	print_endline ("5: \"" ^ (repeat_x 5) ^ "\"")
