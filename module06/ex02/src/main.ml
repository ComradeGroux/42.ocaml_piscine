let () =
	let hum = new People.people "Marco" in
	let doc = new Doctor.doctor "Paulo" 42 hum in
	let dal = new Dalek.dalek in
	dal#talk ;
	dal#talk ;
	dal#talk ;
	dal#talk ;
	dal#talk ;
	dal#talk ;
	print_endline "------------------";
	print_endline dal#to_string;
	print_endline "------------------";
	doc#die ;
	dal#exterminate hum;
	print_endline dal#to_string;
	print_endline doc#to_string;
	doc#take_damage 42;
	print_endline doc#to_string;
	doc#should_regen;
	print_endline doc#to_string;
