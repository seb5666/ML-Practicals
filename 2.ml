fun nfold f n x =
	if n = 0
		then x
	else if n = 1
		then (f x)
	else
	 	nfold f (n-1) (f x);	

fun add a b = nfold (fn n => n + 1) a b;
fun mult x:int y = nfold (add x x) x y;