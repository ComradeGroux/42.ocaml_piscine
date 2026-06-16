let ft_test_sign n = 
	if n < 0 then
		print_endline "negative"
	else
		print_endline "positive"

let () = 
	print_string "Test with [420]: ";
	ft_test_sign 420;

	print_string "Test with [-42]: ";
	ft_test_sign (-42);

	print_string "Test with [  0]: ";
	ft_test_sign 0
