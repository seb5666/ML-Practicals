(*
spb61
Sebastian Borgeaud
Fitzwilliam College
*)

type color = int * int * int;
type xy = int * int;
type image = (color Array.array) Array.array;

fun image ((w,h) :xy) (c :color) :image = Array.tabulate(h, fn j => Array.tabulate(w, fn i => c));

fun size (im :image) = (Array.length(Array.sub(im,0)),Array.length(im));

fun drawPixel (im :image) ((x,y) : xy) c = Array.update(Array.sub(im,y), x, c ); 

fun toPPM  (image :image) filename =
	let val oc = TextIO.openOut filename
		val (w,h) = size image
		fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i)
	in
		TextIO.output(oc, "P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
		Array.app (fn row => Array.app(fn (a,b,c) => TextIO.output(oc, format4 a ^ format4 b ^ format4 c )) row)  image;
		TextIO.closeOut oc
	end;	


fun drawHLine (im :image) ((x,y) :xy) length c =
	let 
		val i = ref 0
	in
		while (!i < length) do
			((drawPixel im ((x + (!i)), y) c); (i := (!i) + 1))
	end;
	
(*Exercise 5*)

fun drawAll (colouring :xy -> color) (image :image) =
	Array.appi 
		(fn (y, row) => (
			Array.appi
				(fn (x, c) => (drawPixel image (x,y) (colouring (x,y))))
				row
			)
		)
		image;
(*
It would be useless to return the new new image, because the old image has been changed to it (it is mutable)
*)
		
fun gradient (x,y) = (((x div 30) * 30) mod 256, 0, ((y div 30) * 30) mod 256);

fun gradImage () = 
	let 
		val image = image (640,480) (0,0,0)
	in
		(drawAll gradient image); (toPPM  image "gradient.ppm")
	end;

fun mandelbrot maxIter (x,y) = 
	let fun solve (a,b) c =
		if c = maxIter then 1.0
		else
			if (a*a + b*b <= 4.0) then
				solve (a*a - b*b + x, 2.0*a*b + y) (c+1)
			else (real c) / (real maxIter)
	in
		solve (x,y) 0
	end;

fun chooseColour n =
	let
		val r = round ((Math.cos n) * 255.0)
		val g = round ((Math.cos n) * 255.0)
		val b = round ((Math.sin n) * 255.0)
	in
		(r,g,b)
	end;

fun rescale ((w,h) :xy) (cx, cy, s) ((x,y) :xy) =
	let
		val p = s * (real(x)/real(w) - 1.0/2.0) + cx
		val q = s * (real(y)/real(h) - 1.0/2.0) + cy
	in
		(p,q)
	end;
	
	
fun compute (cx,cy,s) =
	let 
		val image = image (640,640) (0,0,0)
		fun f (x,y) = chooseColour (mandelbrot 200 (rescale (size(image)) (cx, cy, s) (x,y)))
	in
		(drawAll f image);
		(toPPM  image "mandelbrot.ppm")
	end;

(*
type color = int * int * int
type xy = int * int
type image = color array array
val image = fn: xy -> color -> image
val size = fn: image -> int * int
val drawPixel = fn: image -> xy -> color -> unit
val toPPM = fn: image -> string -> unit
val drawHLine = fn: image -> xy -> int -> color -> unit
val drawAll = fn: (xy -> color) -> image -> unit
val gradient = fn: int * int -> int * int * int
val gradImage = fn: unit -> unit
val mandelbrot = fn: int -> real * real -> real
val chooseColour = fn: real -> int * int * int
val rescale = fn: xy -> real * real * real -> xy -> real * real
val compute = fn: real * real * real -> unit
val it = (): unit

TIME SPENT: 5 hours
*)