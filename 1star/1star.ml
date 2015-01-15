fun sumt(n) : real = 
	let fun sum_aux(x, n) :real =
		if n = 0 then 0.0 else x + sum_aux(x/2.0, n-1)
	in
		sum_aux(1.0,n)
	end;
	
sumt(1);
sumt(2);
sumt(3);
sumt(6);
sumt(9);
sumt(16);
sumt(900);

fun eapprox(n) : real =
	let
		fun e_aux(x, i) : real =
			if i = n then 0.0
			else x + e_aux(x/real(i+1), i+1)
	in
		e_aux(1.0, 0)
	end;
	
eapprox(1);
eapprox(2);
eapprox(3);
eapprox(7);
eapprox(9);
eapprox(99);
eapprox(999);

fun exp(z,n) : real =
	let
		fun exp_aux(x, i) : real =
			if i = n then 0.0
			else 
				if i = 0 then 1.0 + exp_aux(x, i+1)
				else x + exp_aux(x*z/real(i+1), i+1)
	in
		exp_aux(z, 0)
	end;
	
exp(2.0,1);
exp(2.0,2);
exp(2.0,3);
exp(2.0,4);
exp(2.0,5);
exp(2.0,6);
exp(2.0,10);
exp(2.0,100);