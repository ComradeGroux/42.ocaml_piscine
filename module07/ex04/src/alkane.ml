class virtual alkane n = object (self)
	inherit Molecule.molecule
		(
			match n with
			| 1 -> "Methane"
			| 2 -> "Ethane"
			| 3 -> "Propane"
			| 4 -> "Butane"
			| 5 -> "Pentane"
			| 6 -> "Hexane"
			| 7 -> "Heptane"
			| 8 -> "Octane"
			| 9 -> "Nonane"
			| 10 -> "Decane"
			| 11 -> "Undecane"
			| 12 -> "Dodecane"
			| _ -> "Unknown Alkane"
		)
		(
			let rec make_atoms_list number acc =
				if number <= 0 then
					[]
				else
					acc::make_atoms_list (number - 1) acc
				in let c = make_atoms_list n (new Atom.carbon)
				in let h = make_atoms_list (2 * n + 2) (new Atom.hydrogen)
				in c@h
		)
end

class methane = object
	inherit alkane 1
end

class ethane = object
	inherit alkane 2
end

class propane = object
	inherit alkane 3
end

class butane = object
	inherit alkane 4
end

class pentane = object
	inherit alkane 5
end

class hexane = object
	inherit alkane 6
end

class heptane = object
	inherit alkane 7
end

class octane = object
	inherit alkane 8
end

class nonane = object
	inherit alkane 9
end

class decane = object
	inherit alkane 10
end

class undecane = object
	inherit alkane 11
end

class dodecane = object
	inherit alkane 12
end
