type color = int * int * int;
type xy = int * int;
type image = (color Array.array) Array.array;

fun image w h (c :color) :image = Array.tabulate(w, fn j => Array.tabulate(h, fn i => c));

fun size (im :image) = (Array.length(im), Array.length(Array.sub(im,0)));

fun drawPixel (im :image) x y c = Array.update(Array.sub(im,y), x, c ); 

fun toPPM  (image :image) filename =
	let val oc = TextIO.openOut filename
		val (w,h) = size image
		fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i)
	in
		TextIO.output(oc, "P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
		Array.app (fn row => Array.app(fn (a,b,c) => TextIO.output(oc, format4 a ^ format4 b ^ format4 c )) row)  image;
		TextIO.closeOut oc
	end;	


fun drawHLine (im :image) x y length c =
	let 
		val i = ref 0
	in
		while (!i < length) do
			((drawPixel im (x + (!i)) y c); (i := (!i) + 1))
	end;
