
#use "evalg03.ml";;


let expr1 = 
  Let(Ide "y", 
      Eint 755,
      Let(
        Ide "f1",
        Fun(Ide "x",
            Fun(Ide "z",
                Val(Ide "y"))),
        Let(Ide "y", 
            Eint 3,
            (Appl(
               Appl(Val(Ide "f1"), Eint 321),
               Eint 12)))));;
            
(* Equivalente in pseudo-qualsiasi *)
(*

int y = 755;
fun f1 (x) =
{	
	return fun (z) =
	{
		return y;	
	}	
}

{
	int y = 3;
	(f1 321) 12;
}
*)

(* Valore atteso: Int 755 *)
sem expr1 emptyenv;;
