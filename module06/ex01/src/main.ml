let () =
	let dr = new Doctor.doctor "Who" 42 "test" in
	dr#talk;
	dr#die;
	print_endline ("this is the to_string function: '" ^ dr#to_string ^ "'");
	print_newline ();
	dr#travel_in_time 10 11;
	dr#use_sonic_screwdriver;
	print_endline ("this is the to_string function: '" ^ dr#to_string ^ "'");
	(* dr#regenerate *)
