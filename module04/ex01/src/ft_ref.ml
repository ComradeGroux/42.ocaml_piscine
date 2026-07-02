type 'a ft_ref = { mutable contents : 'a }
let return r = { contents = r }
let get r = r.contents
let set r v = r.contents <- v
let bind (r: 'a ft_ref) (f: 'a -> 'b ft_ref) =  f r.contents

let () =
	let x = 42 in
	let ref_x = return x in
	print_endline (string_of_int (get ref_x));
	set ref_x 21;
	print_endline (string_of_int (get ref_x));
	let new_ref = bind ref_x (fun v -> return (string_of_int (v * 3))) in
	print_endline (get new_ref)
