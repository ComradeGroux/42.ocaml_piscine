class virtual reaction (start : (Molecule.molecule * int) list) (finish : (Molecule.molecule * int) list) = object (self)
	method virtual balance : reaction

	method get_start = start
	method get_result = finish

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
			in let sorted_atoms = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) (count molecules [])
			in let combine acc elem =
				match acc with
				| [] -> [elem]
				| (s, n)::tail ->
					if s = fst elem then
						(s, n + snd elem)::tail
					else
						elem::(s,n)::tail
			in List.rev (List.fold_left combine [] sorted_atoms)
		in (count_atoms start = count_atoms finish)

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

class alkane_combustion (start : Alkane.alkane list) = 
	let rec regroup_duplicate lst acc =
		match lst with
		| [] -> acc
		| head::tail -> if List.exists (fun (x, _) -> x#equals head) acc then begin
							let tmp = List.map (fun (x, y) -> if x#equals head then
																(x, y + 1)
															else
																(x, y)
												) acc
							in regroup_duplicate tail tmp
						end else
							regroup_duplicate tail (((head :> Molecule.molecule), 1)::acc)
	in let alkanes_molecules = regroup_duplicate start []
	in let start = (((new Molecule.dioxygen) :> Molecule.molecule), 1)::alkanes_molecules
	in let finish = [(((new Molecule.carbon_dioxide) :> Molecule.molecule), 1) ; (((new Molecule.water) :> Molecule.molecule), 1)]
	in object (self)
		inherit reaction start finish as super

		method get_start =
			if self#is_balanced then
				start
			else
				raise (Failure "Reaction is not balanced")
		method get_result =
			if self#is_balanced then
				finish
			else
				raise (Failure "Reaction is not balanced")

		method balance =
			let rec get_every_atoms lst acc =
				match lst with
				| [] -> acc
				| (molecule, n)::tail -> 
					let rec multiply_molecule count ac =
						if count > 0 then
							multiply_molecule (count - 1) (molecule#get_atoms @ ac)
						else
							ac
					in get_every_atoms tail ((multiply_molecule n []) @ acc)
			in let atoms = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) (get_every_atoms start [])
			in let combine acc elem =
				match acc with
				| [] -> [elem]
				| (str, n)::tail ->
					if str = fst elem then
						(str, n + snd elem)::tail
					else
						elem::(str,n)::tail
			in let atoms = List.rev (List.fold_left combine [] atoms)

			in let count_carbon = try List.assoc "C" atoms with Not_found -> 0
			in let count_hydrogen = try List.assoc "H" atoms with Not_found -> 0
			in let k = if count_hydrogen mod 4 = 0 then 1 else 2
			in let total_carbon = count_carbon * k
			in let total_hydrogen = count_hydrogen * k

			in let coeff_co2 = total_carbon
			in let coeff_h2o = total_hydrogen / 2
			in let coeff_o2 = coeff_co2 + total_hydrogen / 4
			in let new_start = (List.map (fun (m, c) -> (m, c * k)) alkanes_molecules) @ [(((new Molecule.dioxygen) :> Molecule.molecule), coeff_o2)]
			in let new_finish = [ (((new Molecule.carbon_dioxide) :> Molecule.molecule), coeff_co2) ; (((new Molecule.water) :> Molecule.molecule), coeff_h2o) ]

			in (object
				inherit reaction new_start new_finish
				method balance = (self :> reaction)
			end)
end
