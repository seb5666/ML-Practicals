datatype 'a stream = Cons of 'a * (unit -> 'a stream);

fun head (Cons(x,_)) = x;

fun tail (Cons(_,xf)) = xf();

fun map2 f ys xs = Cons((f (head ys) (head xs)), fn () => map2 f (tail ys) (tail xs));
