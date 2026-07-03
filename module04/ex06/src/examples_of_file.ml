let examples_of_file filePath =
	if Sys.file_exists filePath = false then invalid_arg "File does not exist" else ();
	let ifd = open_in filePath in
	let file = ref [] in
	begin
		try
			while true do
				let line = input_line ifd in
				let splittedLine = String.split_on_char ',' line in
				let acc = ref [] in
				let lineLength = List.length splittedLine in
				for i = 0 to lineLength - 2 do
					ignore (acc := (float_of_string (List.nth splittedLine i))::!acc);
				done;
				file := (Array.of_list (List.rev !acc), (List.nth splittedLine ((List.length splittedLine) - 1)))::!file
			done
		with
		| End_of_file -> ()
	end;
	close_in ifd ;
	List.rev !file




let print_examples lst =
	match lst with
	| head::tail -> begin
		match head with
		| (arr , str) -> begin
			for i = 0 to Array.length arr - 1 do
				print_float arr.(i) ;
				print_string " ; "
			done;
			print_endline str
		end
	end
	| [] -> ()

let () =
	if Array.length (Sys.argv) <> 2 then begin
		prerr_endline "You should provide the CSV's filename" ;
		exit 1
	end else () ;

	print_examples (examples_of_file Sys.argv.(1))
