class ['a] army = object
	val mutable _members : 'a list = []

	method add instance = _members <- instance::_members
	method delete =
		match _members with
		| [] -> ()
		| _::tail -> _members <- tail
end
