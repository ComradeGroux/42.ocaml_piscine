let ft_rot_n n str =
	let n = n mod 26 in
	let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
	let rotate c =
		if is_alpha c == true then
			let tmp = char_of_int ((int_of_char c) + n) in
			if is_alpha tmp == true then
				tmp
			else
				char_of_int (int_of_char(tmp) - 26)
		else
			c

	in String.map rotate str

let () =
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 42 "0123456789");
	print_endline (ft_rot_n 2 "OI2EAS67B9");
	print_endline (ft_rot_n 0 "Damned !");
	print_endline (ft_rot_n 42 "");
	print_endline (ft_rot_n 1 "NBzlk qnbjr !");
