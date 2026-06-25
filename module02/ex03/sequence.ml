let sequence n =
	if n < 0 then
		""
	else
		let rec getStr acc list =
			match list with
			| [] -> acc
			| head::tail -> acc ^ (string_of_int head) ^ (getStr acc tail)
		in let rec countNum count num list =
			match list with
			| [] -> [count; num]
			| head::tail when head = num -> countNum (count + 1) num tail
			| head::tail -> count::num::(countNum 1 head tail)
		in let rec loop n list =
			if n = 0 then
				getStr "" list
			else
				match list with
				| [] -> ""
				| head::tail -> loop (n - 1) (countNum 1 head tail)
		in loop n [1]

let () =
	print_endline ("-42: " ^ sequence (-42));
	print_endline ("  0: " ^ sequence 0);
	print_endline ("  1: " ^ sequence 1);
	print_endline ("  2: " ^ sequence 2);
	print_endline ("  3: " ^ sequence 3);
	print_endline ("  4: " ^ sequence 4)
