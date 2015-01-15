fun toPPM image filename =
	let val oc = TextIO.openOut filename
		val (w,h) = size image
	in
		TextIO.ouput(oc, "P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n")
		TextIO.output(oc, "")
		TextIO.closeOut oc
	end;		