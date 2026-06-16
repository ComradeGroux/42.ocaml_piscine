let ft_string_all (f: char -> bool) str = 
	let rec check_pos i =
		if i >= (String.length str) - 1 then 
			true
		else
			if f (String.get str i) == true then
				check_pos (i + 1)
			else
				false

	in check_pos 0

let () = 
	let is_digit c = c >= '0' && c <= '9' in
	if ft_string_all is_digit "0123456789" == true then
		print_endline "True"
	else
		print_endline "False";

	if ft_string_all is_digit "01234e56789" == true then
		print_endline "True"
	else
		print_endline "False"
