let extract_jokes file =
	let lst = ref [] in
	let ifd = open_in file in
	begin
		try
			while true do
				let line = input_line ifd in
				lst := line :: !lst
			done
		with End_of_file -> ()
	end;
	close_in ifd ;
	Array.of_list (List.rev !lst)

let main jokes =
	print_endline jokes.((Random.int (Array.length jokes)))


let checking_arg argv =
	if Array.length argv <> 2 then
		invalid_arg "You should provide the file containing the jokes"
	else if Sys.file_exists argv.(1) = false then
		invalid_arg "File does not exist"

let () =
	begin
		try checking_arg Sys.argv with
		| Invalid_argument msg -> prerr_endline msg ; exit 1
	end;
	Random.self_init ();
	main (extract_jokes Sys.argv.(1))
