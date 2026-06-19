let rec ft_sum (f: int -> float) ?(res=0.0) lower ?(i=lower) upper =
	if lower > upper then
		nan
	else if i <= upper then
		ft_sum f ~res:(res +. (f (i))) lower ~i:(i + 1) upper
	else
		res

let () =
	print_float (ft_sum (fun i -> float_of_int(i * i)) 1 10) ;
	print_newline ()
