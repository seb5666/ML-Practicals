fun last [x] = x
	| last (x::l) = last(l);


last [1,2,3,4,5];
last [2,3,~1];

fun butLast (x::l) =
	let
		fun  butLast_aux([x], temp) = temp
			| butLast_aux (x::l, temp) = x::butLast_aux(l, temp);	
	in
		butLast_aux(x::l, [])
	end;
