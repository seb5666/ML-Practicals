fun allcons (x, []) = []
	| allcons (x, ls::lss) = (x::ls) :: allcons(x,lss);

allcons(6, [[1,2],[2,3],[],[1]]);

fun concat([], ys) = ys
	| concat (x::xs, ys) = concat(xs, x::ys);

concat([[1,2],[3]],[[4,5],[6]]);


fun choose(n, []) = []
	| choose(1, x::xs) = [x] :: choose(1,xs)
	| choose (n, x::xs) = concat(allcons(x, choose(n-1, xs)), choose(n,xs));

val l = [1,2,3,4,5];
choose(3, l);
