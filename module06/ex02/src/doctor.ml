class doctor (name : string) (age : int) (sidekick : People.people) = object
	val _name : string = name
	val _age : int = age
	val _sidekick : People.people = sidekick
	val mutable _hp : int = 100

	method to_string = "Name=" ^ _name ^ "; Age=" ^ string_of_int _age ^ "; Sidekick=" ^ _sidekick#to_string ^ "; HP=" ^ string_of_int _hp
	method talk = print_endline ("I'm " ^ _name ^ "! Do you know Doctor?")
	method die = print_endline "Aaaarghh!"
	method travel_in_time (start: int) (arrival: int) =
		print_endline ( "_______(_@_)_______\n" ^
						"|                 |\n" ^
						"|  POLICE    BOX  |\n" ^
						"|_________________|\n" ^
						" | _____ | _____ |\n" ^
						" | |###| | |###| |\n" ^
						" | |###| | |###| |\n" ^
						" | _____ | _____ |\n" ^
						" | || || | || || |\n" ^
						" | ||_|| | ||_|| |\n" ^
						" | _____ |$_____ |\n" ^
						" | || || | || || |\n" ^
						" | ||_|| | ||_|| |\n" ^
						" | _____ | _____ |\n" ^
						" | || || | || || |\n" ^
						" | ||_|| | ||_|| |\n" ^
						" |       |       |\n" ^
						" *****************")
	method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
	method private regenerate = _hp <- 100

	initializer print_endline "A doctor was created !"
end
