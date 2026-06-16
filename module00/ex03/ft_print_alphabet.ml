let ft_print_alphabet = 
	let rec print_letter c = 
		if c <= int_of_char 'z' then begin
			print_char (char_of_int c);
			print_letter (c + 1);
		end else
			print_char '\n';
	in print_letter (int_of_char 'a')

let () = ft_print_alphabet
