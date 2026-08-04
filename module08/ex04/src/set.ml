type 'a t = 'a list

let return (x: 'a) : 'a t = [x]

let bind (a: 'a t) (f: 'a -> 'b t) : 'b t = List.sort_uniq compare (List.concat_map f a)

let union (a: 'a t) (b: 'a t) : 'a t = List.sort_uniq compare (a @ b)
(*	match a with
	| [] -> b
	| a -> begin
		let rec read lst (b, t_b) res =
			if lst = [] && t_b = [] then res
			else
				match lst with
				| [] -> res
				| head::snd::tail ->
					let c = compare head b
					in if c <= 0 then read (snd::tail) (b, t_b) (head::res)
					else read t_b (head, snd::tail) (b::res)
				| head::[] ->
					let c = compare head b
					in if c <= 0 then (head::(b::t_b)@res)
					else read t_b (head, []) (b::res)
		in match b with
		| head::tail -> List.sort_uniq compare (read a (head, tail) [])
		| [] -> a
	end*)

let inter (a: 'a t) (b: 'a t) : 'a t =
	let rec read a b res =
		match a with
		| [] -> res
		| head::tail ->
			if List.exists (fun x -> x = head) b then
				read tail b (head::res)
			else
				read tail b res
	in List.sort_uniq compare (read a b [])

let diff (a: 'a t) (b: 'a t) : 'a t =
	let rec read a b res =
		match a with
		| [] -> res
		| head::tail ->
			if List.exists (fun x -> x = head) b then
				read tail b res
			else
				read tail b (head::res)
	(* Les lignes suivantes dependent de l'interpretation de diff *)
	(* Sortir tout ce qui existe dans a et PAS dans b *)
	in List.sort_uniq compare (read a b [])

	(* Sortir tout ce qui existe dans a ET dans b *)
	(* in List.sort_uniq compare (read a b (read b a [])) *)

let filter (a: 'a t) (f: 'a -> bool) : 'a t = List.filter f a

let foreach (a: 'a t) (f: 'a -> unit) : unit = List.iter f a

let for_all (a: 'a t) (f: 'a -> bool) : bool = List.for_all f a

let exists (a: 'a t) (f: 'a -> bool) : bool = List.exists f a
