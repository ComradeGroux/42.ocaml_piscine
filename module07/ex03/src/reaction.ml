class virtual reaction (start : (Molecule.molecule * int) list) (finish : (Molecule.molecule * int) list) = object (self)
	method virtual get_start : (Molecule.molecule * int) list
	method virtual get_result : (Molecule.molecule * int) list
	method virtual balance : reaction
	method virtual is_balanced : bool

	method equals (other: reaction) =
		let compare_molecule_list lst1 lst2 =
			let lst1_sorted = List.sort (fun (m1, _) (m2, _) -> compare m1#name m2#name) lst1 in
			let lst2_sorted = List.sort (fun (m1, _) (m2, _) -> compare m1#name m2#name) lst2 in
			let rec check element =
				match element with
				| [], [] -> true
				| (m1, c1)::tail1, (m2, c2)::tail2 -> m1#equals m2 && c1 = c2 && check (tail1, tail2)
				| _ -> false
			in check (lst1_sorted, lst2_sorted)
		in (compare_molecule_list self#get_start other#get_start) && (compare_molecule_list self#get_result other#get_result)
	
	method to_string = 
		let format_molecule lst =
			let parts = List.map (fun (m, c) ->	if c = 1 then m#formula else string_of_int c ^ " " ^ m#formula) lst
			in String.concat " + " parts
		in (format_molecule self#get_start ^ " -> " ^ format_molecule self#get_result)
end

class simple_reaction (start : (Molecule.molecule * int) list) (finish : (Molecule.molecule * int) list) = object (self)
	inherit reaction start finish

	method get_start = start
	method get_result = finish

	method balance = (self :> reaction)
	method is_balanced =
		let count_atoms molecules =
			let rec count lst acc =
				match lst with
				| [] -> acc
				| (molecule, c)::tail ->
					let rec for_each_atoms atoms ac =
						match atoms with
						| [] -> ac
						| (symbol, co)::t -> for_each_atoms t ((symbol, co * c)::ac)
					in count tail ((for_each_atoms molecule#get_atoms []) @ acc)
			in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) (count molecules [])
		in (count_atoms start = count_atoms finish)
end
