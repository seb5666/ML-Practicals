fun s x = x+1; 

fun add a 0 = a | add a b = s (add a (b-1));  

fun mult a 1 = a | mult a b = add a (mult a (b-1));

fun power a 0 = 1 | power a b = mult a (power a (b-1));
