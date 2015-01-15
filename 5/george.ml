fun drawAll (f : (xy -> color)) (im:image) = 
		let 
			val (w,h) = size im;
			val y = ref 0;
		in 
			while (!y<h) do (
			Array.update(im, !y, Array.tabulate( w, fn i => f (i, !y)));
			
			y:= !y +1
			
			)
		end;
		
		
fun gradient (x,y) =
	(((x div 30) * 30) mod 256, 0, ((y div 30) * 30) mod 256);
	

fun gradImage ()= 
	let
	val im = image (1024,720) (0,0,0);
	in 
	drawAll gradient im;
	toPPM im "gradient.ppm"
	end;
	
		
		
fun mandelbrot maxIter (x,y) =
	let fun solve (a,b) c =
		if c = maxIter then 1.0
			else
			if (a*a + b*b <= 4.0) then
				solve (a*a - b*b + x,2.0*a*b + y) (c+1)
			else (real c)/(real maxIter)
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
	
fun rescale ((w,h):xy) (cx,cy,s) ((x,y):xy) = 
	let 
		val p = s*(real x / real w - 0.5) + cx;
		val q = s*(real y / real h - 0.5) + cy;
	in 
		(p,q)
	end;
	
fun compute (cx, cy, s) = 
	let 
	val im = image (640, 480) (0,0,0);
	fun auxfun (x,y) =
		chooseColour (mandelbrot 150 (rescale (640, 480) (cx,cy,s) (x,y)));
			
	in
	drawAll auxfun im;
	toPPM im "mandelbrot.ppm"
	end;