let () =
	let p1 = new People.people "p1" in
	let p2 = new People.people "p2" in
	let p3 = new People.people "p3" in
	let p4 = new People.people "p4" in
	let p5 = new People.people "p5" in
	let p6 = new People.people "p6" in
	let army_people : People.people Army.army = new Army.army in
	army_people#add p1 ;
	army_people#add p2 ;
	army_people#add p3 ;
	army_people#add p4 ;
	army_people#add p5 ;
	army_people#add p6 ;
	army_people#delete ;
	army_people#delete ;
	army_people#delete ;
	army_people#delete ;
	army_people#delete ;
	army_people#delete ;
	army_people#delete ;

	let do1 = new Doctor.doctor "do1" 1 p1 in
	let do2 = new Doctor.doctor "do2" 2 p2 in
	let do3 = new Doctor.doctor "do3" 3 p3 in
	let do4 = new Doctor.doctor "do4" 4 p4 in
	let do5 = new Doctor.doctor "do5" 5 p5 in
	let do6 = new Doctor.doctor "do6" 6 p6 in
	let army_doctor : Doctor.doctor Army.army = new Army.army in
	army_doctor#add do1 ;
	army_doctor#add do2 ;
	army_doctor#add do3 ;
	army_doctor#add do4 ;
	army_doctor#add do5 ;
	army_doctor#add do6 ;
	army_doctor#delete ;
	army_doctor#delete ;
	army_doctor#delete ;
	army_doctor#delete ;
	army_doctor#delete ;
	army_doctor#delete ;
	army_doctor#delete ;

	let da1 = new Dalek.dalek in
	let da2 = new Dalek.dalek in
	let da3 = new Dalek.dalek in
	let da4 = new Dalek.dalek in
	let da5 = new Dalek.dalek in
	let da6 = new Dalek.dalek in
	let army_dalek : Dalek.dalek Army.army = new Army.army in
	army_dalek#add da1 ;
	army_dalek#add da2 ;
	army_dalek#add da3 ;
	army_dalek#add da4 ;
	army_dalek#add da5 ;
	army_dalek#add da6 ;
	army_dalek#delete ;
	army_dalek#delete ;
	army_dalek#delete ;
	army_dalek#delete ;
	army_dalek#delete ;
	army_dalek#delete ;
	army_dalek#delete ;
