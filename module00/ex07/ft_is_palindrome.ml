let ft_is_palindrome str = 
	let rec check_char i = 
		if i >= (((String.length str) + 1) / 2) then
			true
		else if String.get str i == String.get str (((String.length str) - 1) - i) then
			check_char (i + 1)
		else
			false

	in check_char 0

let () = 
	print_string "madam: ";
	if ft_is_palindrome "madam" == true then
		print_endline "True"
	else
		print_endline "False";

	print_string "radar: ";
	if ft_is_palindrome "radar" == true then
		print_endline "True"
	else
		print_endline "False";

	print_string "kAyak: ";
	if ft_is_palindrome "kAyak" == true then
		print_endline "True"
	else
		print_endline "False";

	print_string "test: ";
	if ft_is_palindrome "test" == true then
		print_endline "True"
	else
		print_endline "False";

	print_string "\"\": ";
	if ft_is_palindrome "" == true then
		print_endline "True"
	else
		print_endline "False";

