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
	in let alkanes_molecules = regroup_duplicate (start :> Molecule.molecule list) []
	in let reg_start = (((new Molecule.dioxygen) :> Molecule.molecule), 1)::alkanes_molecules
	in let finish = [(((new Molecule.carbon_dioxide) :> Molecule.molecule), 1) ; (((new Molecule.water) :> Molecule.molecule), 1)]
	in object (self)
		inherit Reaction.reaction reg_start finish as super

		method get_start =
			if self#is_balanced then
				reg_start
			else
				raise (Failure "Reaction is not balanced")
		method get_result =
			if self#is_balanced then
				finish
			else
				raise (Failure "Reaction is not balanced")

		method balance =
			let atoms = self#get_every_atoms_combined

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
				inherit Reaction.reaction new_start new_finish
				method balance = (self :> Reaction.reaction)
			end)

		method private get_every_atoms lst acc =
			match lst with
			| [] -> acc
			| (molecule, n)::tail -> 
				let rec multiply_molecule count ac =
					if count > 0 then
						multiply_molecule (count - 1) (molecule#get_atoms @ ac)
					else
						ac
				in self#get_every_atoms tail ((multiply_molecule n []) @ acc)

		method private get_every_atoms_combined =
			let atoms = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) (self#get_every_atoms reg_start [])
			in let combine acc elem =
				match acc with
				| [] -> [elem]
				| (str, n)::tail ->
					if str = fst elem then
						(str, n + snd elem)::tail
					else
						elem::(str,n)::tail
			in List.rev (List.fold_left combine [] atoms)

		method get_incomplete_results =
			let atoms = self#get_every_atoms_combined
			in let total_carbon = try List.assoc "C" atoms with Not_found -> 0
			in let total_hydrogen = try List.assoc "H" atoms with Not_found -> 0
			in let coeff_h2o = total_hydrogen / 2
			in let min_o2 = coeff_h2o / 2
			in let min_o2 = if total_hydrogen mod 4 != 0 then min_o2 + 1 else min_o2
			in let max_o2 = total_carbon + total_hydrogen / 4
			in let rec calculate_results o2_quantity res =
				if o2_quantity = max_o2 then
					res
				else begin
					let dispo_oxygen = (o2_quantity * 2) - coeff_h2o
					in let rec found_solutions coeff_co2 results =
						if coeff_co2 = total_carbon then
							results
						else begin
							let coeff_co = dispo_oxygen - (2 * coeff_co2)
							in let coeff_c = total_carbon - coeff_co2 - coeff_co
							in if (coeff_co >= 0 && coeff_c >= 0) then
								if (coeff_co = 0 && coeff_c = 0) then
									found_solutions (coeff_co2 + 1) results
								else begin
									let reaction_lst = [((new Molecule.water :> Molecule.molecule), coeff_h2o)]
									in let reaction_lst = match coeff_co2 with
									| 0 -> reaction_lst
									| _ -> (((new Molecule.carbon_dioxide :> Molecule.molecule), coeff_co2)::reaction_lst)
									in let reaction_lst = match coeff_co with
									| 0 -> reaction_lst
									| _ -> (((new Molecule.carbon_monoxide :> Molecule.molecule), coeff_co)::reaction_lst)
									in let reaction_lst = match coeff_c with
									| 0 -> reaction_lst
									| _ -> (((new Molecule.carbon :> Molecule.molecule), coeff_c)::reaction_lst)
									in found_solutions (coeff_co2 + 1) ((o2_quantity , reaction_lst)::results)
								end
							else
								found_solutions (coeff_co2 + 1) results
						end
					in let curr_reactions = found_solutions 0 []
					in calculate_results (o2_quantity + 1) (curr_reactions@res)
				end
			in calculate_results min_o2 []
end
