type project = string * string * int

let zero : project = ("", "", 0)

let combine (p1 : project) (p2 : project) : project =
	if p1 = zero then p2
	else if p2 = zero then p1
	else
		let (n1, _, g1) = p1
		in let (n2, _, g2) = p2
		in let average = (g1 + g2) / 2
		in let status = if average > 80 then "succeed" else "failed"
		in (n1 ^ n2, status, average)

let fail ((n, _, _): project) : project = (n, "failed", 0)

let success ((n, _, _): project) : project = (n, "succeed", 80)
