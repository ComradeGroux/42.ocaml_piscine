class virtual atom name symbol atomic_number = object (self)
	val _name : string = name
	val _symbol : string = symbol
	val	_atomic_number : int = atomic_number

	method to_string = "Name: " ^ _name ^ "\tSymbol: " ^ _symbol ^ "\tAtomic number: " ^ string_of_int _atomic_number
	method equals (other : atom) = (self#get_name = other#get_name) && (self#get_symbol = other#get_symbol) && (self#get_atomic_number = other#get_atomic_number)

	method get_name = _name
	method get_symbol = _symbol
	method get_atomic_number = _atomic_number
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
