class virtual molecule (name: string) (atoms: Atom.atom list) = object (self)
	method name = name
	method formula = self#create_formula
	method to_string = "Name: " ^ name ^ "\tFormula: " ^ self#formula
	method equals (other: molecule) = (self#name = other#name) && (self#formula = other#formula)

	method get_atoms =
		let rec count_rec_symbol count lst acc =
			match lst with
			| head::next::tail when head = next		-> count_rec_symbol (count + 1) (next::tail) acc
			| head::next::tail when head != next	-> count_rec_symbol 1 (next::tail) (acc @ [(head, count)])
			| head::[] 								-> count_rec_symbol 1 [] (acc @ [(head, count)])
			| _ -> acc
		in count_rec_symbol 1 self#sort_atoms []

	method private create_formula = 
		let rec count_rec_symbol count lst acc =
			match lst with
			| head::next::tail when head = next		-> count_rec_symbol (count + 1) (next::tail) acc
			| head::next::tail when head != next	-> count_rec_symbol 1 (next::tail) (acc ^ head ^ (if count > 1 then string_of_int count else ""))
			| head::[] 								-> count_rec_symbol 1 [] (acc ^ head ^ (if count > 1 then string_of_int count else ""))
			| _ -> acc
		in count_rec_symbol 1 self#sort_atoms ""
	method private sort_atoms =
		let lst_symbol = List.map (function x -> x#symbol) atoms in
		let carbon = List.filter (function x -> x = "C") lst_symbol in
		let hydrogen = List.filter (function x -> x = "H") lst_symbol in
		let others = List.filter (function x -> x <> "C" && x <> "H") lst_symbol in
		let sorted = List.sort compare others in
		carbon@hydrogen@sorted
end

class water = object
	inherit molecule "Water" [ new Atom.hydrogen ; new Atom.oxygen ; new Atom.hydrogen ]
end

class carbon_dioxide = object
	inherit molecule "Carbon Dioxide" [ new Atom.oxygen ; new Atom.oxygen ; new Atom.carbon ]
end

class trinitrotoluene = object
	inherit molecule "Trinitrotoluene"	[ new Atom.nitrogen ; new Atom.nitrogen ; new Atom.nitrogen ;
										  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
										  new Atom.oxygen ; new Atom.oxygen ; new Atom.oxygen ; new Atom.oxygen ; new Atom.oxygen ; new Atom.oxygen ;
										  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon
										]
end

class ozone = object
	inherit molecule "Ozone" [ new Atom.oxygen ; new Atom.oxygen ; new Atom.oxygen ]
end

class methane = object
	inherit molecule "Methane" [ new Atom.carbon ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ]
end

class dihydrogen = object
	inherit molecule "Dihydrogen" [ new Atom.hydrogen ; new Atom.hydrogen ]
end

class dioxygen = object
	inherit molecule "Dioxygen" [ new Atom.oxygen ; new Atom.oxygen ]
end
