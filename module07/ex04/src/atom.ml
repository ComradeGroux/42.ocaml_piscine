class virtual atom name symbol atomic_number = object (self)
	method name = name
	method symbol = symbol
	method atomic_number = atomic_number

	method to_string = "Name: " ^ name ^ "\tSymbol: " ^ symbol ^ "\tAtomic number: " ^ string_of_int atomic_number
	method equals (other : atom) = (self#name = other#name) && (self#symbol = other#symbol) && (self#atomic_number = other#atomic_number)
end

class hydrogen = object
	inherit atom "Hydrogen" "H" 1
end

class carbon = object
	inherit atom "Carbon" "C" 6
end

class oxygen = object
	inherit atom "Oxygen" "O" 8
end

class helium = object
	inherit atom "Helium" "He" 2
end

class silver = object
	inherit atom "Silver" "Ag" 47
end

class gold = object
	inherit atom "Gold" "Au" 79
end

class nitrogen = object
	inherit atom "Nitrogen" "N" 7
end

class sodium = object
	inherit atom "Sodium" "Na" 11
end

class chlorine = object
	inherit atom "Chlorine" "Cl" 17
end
