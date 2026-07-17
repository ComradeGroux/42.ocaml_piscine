let () =
	let test = new People.people "test" in
	test#talk;
	test#die;
	print_endline ("this is the to_string function: '" ^ test#to_string ^ "'")
