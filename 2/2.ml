fun butLast [] = []
	| butLast [x] = []
	| butLast (x::l) = x::butLast(l);

val l = [1,2,3,4,5];

butLast l;
rev(tl(rev l));
butLast[1];


