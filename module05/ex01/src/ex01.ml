module HashString = struct
	type t = String.t

	let equal s1 s2 =
		compare s1 s2 = 0

	let hash str =
		let rec fnv_1a str acc i =
			if i = String.length str then
				acc
			else begin
				let ac = acc lxor int_of_char (String.get str i) in
				fnv_1a str (ac * 16777619) (i + 1)
			end
		in fnv_1a str 2166136261 0
end

module StringHashtbl = Hashtbl.Make (HashString)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
