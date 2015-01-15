datatype 'a stream = Cons of 'a * (unit -> 'a stream);

fun from k = Cons(k, fn() => from(k+1));

fun nth(s,n) = 
	let 
		fun head (Cons(x,_)) = x
		fun tail (Cons(_,xf)) = xf()
	in
		if n = 1 then head(s) else nth(tail(s), n-1)
	end;


val squares =
	let 
		fun squares_aux(k) = Cons(k*k, fn () => squares_aux(k+1))
	in 
		squares_aux(1)
	end;
