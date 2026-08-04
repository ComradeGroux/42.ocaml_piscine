let print_proj ((name, status, grade): App.project) =
	print_endline ("The project '" ^ name ^ "' was '" ^ status ^ "' with grade " ^ string_of_int grade)

let () =
	print_proj App.zero ;

	let proj_a = ("Alice", "", 0) in
	let proj_b = ("Bob", "", 0) in
	print_proj (App.fail proj_a) ;
	print_proj (App.success proj_b) ;

	let proj_c = ("Charlie", "", 60) in
	let proj_d = ("Dave", "", 70) in
	print_proj (App.combine proj_c proj_d) ;

	let proj_e = ("Eve", "", 90) in
	let proj_f = ("Frank", "", 85) in
	print_proj (App.combine proj_e proj_f) ;

	let proj_g = ("Grace", "", 80) in
	let proj_h = ("Heidi", "", 80) in
	print_proj (App.combine proj_g proj_h) ;
	
	print_newline ();
	print_endline "combine zero something";
	let proj_i = ("Ivan", "succeed", 95) in
	let result_left = App.combine App.zero proj_i in
	print_proj result_left ;

	print_newline ();
	print_endline "combine something zero";
	let result_right = App.combine proj_i App.zero in
	print_proj result_right ;

	print_newline ();
	print_endline "combine zero zero";
	let result_zz = App.combine App.zero App.zero in
	print_proj result_zz ;
