let rec fibonacci ?(res1=0) ?(res2=1) n =
	if n < 0 then
		-1
	else
		if n = 0 then
			res1
		else if n = 1 then
			res2
		else
			fibonacci ~res1:res2 ~res2:(res1 + res2) (n - 1) 

let () =
	print_string "fibo -42: ";
	print_int (fibonacci (-42));
	print_newline ();

	print_string "fibo 1  : ";
	print_int (fibonacci 1);
	print_newline ();

	print_string "fibo 3  : ";
	print_int (fibonacci 3);
	print_newline ();

	print_string "fibo 6  : ";
	print_int (fibonacci 6);
	print_newline ()
