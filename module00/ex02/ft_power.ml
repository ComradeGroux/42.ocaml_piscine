let rec ft_power x expo = 
	if expo = 0 then
		1
	else
		x * ft_power x (expo - 1)

let () = 
	print_string "5^2: ";
	print_int (ft_power 5 2);
	print_newline ();
	
	print_string "2^8: ";
	print_int (ft_power 2 8);
	print_newline ();

	print_string "0^4: ";
	print_int (ft_power 0 4);
	print_newline ();

	print_string "3^0: ";
	print_int (ft_power 3 0);
	print_newline ()

