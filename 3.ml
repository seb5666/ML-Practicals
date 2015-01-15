fun s x = x+1;
fun add a 0 = a |add  a b = s(add a (b-1));
fun mult a 1 = a | mult a b = add a (mult a (b-1)) ;
fun pow a 0 = 1 | pow a b = mult a (pow a (b-1)); 

fun nfold f 0 x = x
	|nfold f 1 x = (f x)
	|nfold f n x = (nfold f (n-1) (f x));
	
fun add a b = nfold (fn x=>x+1) a b;
fun mult a b = nfold (add a) (b-1) a;
fun pow a b = nfold (mult a) (b-1) a;

datatype 'a stream = Cons of 'a * (unit ->'a stream)

fun from k = Cons(k, fn() => from (k+1));

fun nth (Cons(x,xf)) 1 = x
	|nth (Cons(x,xf)) n = nth (xf()) (n-1);
	
fun squares k = Cons(k*k, fn() => squares (k+1));

fun map2 f (Cons(x,xs)) (Cons(y,ys)) = Cons((f x y), fn() => map2 f (xs()) (ys()))

fun add a b = a + b;

fun tail (Cons(x,xf)) = xf();

fun fibs() = Cons(1, fn() => Cons(1, fn() => map2 add (fibs()) (tail(fibs())) ));

fun merge (Cons(x,xs)) (Cons(y,ys)) = 
	if x > y then Cons(y, fn()=> merge (Cons(x,xs)) (ys()) )
	else
		if x < y then Cons(x, fn() => merge (xs()) (Cons(y,ys)))
		else Cons(x, fn() => merge (xs()) (ys()));
		
fun map f (Cons(x,xs)) = Cons((f x),fn() => map f (xs()));

fun double x = 2*x;
fun triple x = 3*x;

fun ij () = Cons(1, fn() => merge (map double (ij())) (map triple (ij())) );

fun quint x = 5*x;

fun tripleMerge xs ys zs = merge (merge xs ys) zs;

fun ijk() = Cons(1, fn() => tripleMerge (map double (ijk())) (map triple (ijk())) (map quint (ijk())) );