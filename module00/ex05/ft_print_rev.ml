let ft_print_rev str = 
	let rec print_rev i = 
		if i >= 0 then begin
			print_char (String.get str i);
			print_rev (i - 1)
		end else
			print_char '\n'
	in print_rev ((String.length str) - 1)

let () =
	print_string "Hello world = ";
	ft_print_rev "Hello world";

	print_string "42 = ";
	ft_print_rev "42";

	print_string "'' = ";
	ft_print_rev "";
