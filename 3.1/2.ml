fun nfold f n x =
	if n = 0
		then x
	else if n = 1
		then (f x)
	else
	 	nfold f (n-1) (f x);	

fun sum a b= nfold (fn n => n+1) a b;

fun mult a b = nfold (sum a) (b-1) a;

fun power m n = nfold (mult m) (n-1) m;