#use "evtry.ml";;
(* Vari tests *)
let giuste = 
  [Eint 2, Int 2; 
   Echar 'c', Char 'c'; 
   True, Bool true; 
   False, Bool false; 
   Empty, List [];
   Sum(Eint 2, Eint 3), Int 5;
   Diff(Eint 2, Eint 3), Int (-1);
   Times(Eint 2, Eint 3), Int 6; 
   And(True, False), Bool false;
   Or(False, True), Bool true; 
   Not(True), Bool false;
   Less(Eint 2, Eint 3), Bool true; 
   Less(Sum(Eint 2, Eint 3), Eint 3), Bool false; 
   Less(Diff(Eint 2, Eint 3), Eint 6), Bool true;
   Eq(Eint 2, Eint 4), Bool false; 
   Eq(Eint 3, Eint 3), Bool true; 
   Eq(Echar 'c', Echar 'd'), Bool false; 
   Eq(True, False), Bool false;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 2, Echar 'c')), Bool true;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 3214, Echar 'd')), Bool false;
   Eq(Cons(Eint 3, Empty), Cons(Eint 3, Empty)), Bool true;
   Eq(Cons(Eint 2, Empty), Cons(Eint 3, Empty)), Bool false;
   Eq(Let(Ide "x", Echar 'c', Val (Ide "x")), Echar 'c'), Bool true; 
   Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Eint 3), Bool false;
   Eq(Ifthenelse(True, Eint 2, Eint 3), Ifthenelse(False, Eint 3, Eint 4)), Bool false;
   Cons(Eint 3, Empty), List [Int 3]; 
   Cons(Echar 'c', Empty), List [Char 'c']; 
   Cons(True, Empty), List [Bool true]; 
   Cons(Eint 2, Cons(Eint 3, Empty)), List [Int 2; Int 3];
   Cons(True, Cons(False, Empty)), List [Bool true; Bool false];
   Cons(Echar 'c', Cons(Echar 'd', Empty)), List [Char 'c'; Char 'd'];
   Cons(Cons(Eint 2, Empty), Cons(Cons(Eint 3, Empty), Empty)), List [List [Int 2]; List [Int 3]];
   Cons(Cons(Let(Ide "x", Echar 'c', Val (Ide "x")), Empty), Cons(Empty, Empty)), List [List[Char 'c']; List[]];
(*   Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty)), List [Closure(;*) 
   Head(Cons(Eint 3, Empty)), Int 3; 
   Head(Cons(Echar 'c', Empty)), Char 'c';
   Head(Cons(True, Empty)), Bool true; 
   Head(Cons(Eint 2, Cons(Eint 3, Empty))), Int 2;
   Head(Cons(True, Cons(False, Empty))), Bool true; 
   Head(Cons(Echar 'c', Cons(Echar 'd', Empty))), Char 'c';
   Tail(Cons(Eint 3, Empty)), List []; 
   Tail(Cons(Echar 'c', Empty)), List [];
   Tail(Cons(True, Empty)), List []; 
   Tail(Cons(Eint 2, Cons(Eint 3, Empty))), List [Int 3];
   Tail(Cons(True, Cons(False, Empty))), List [Bool false];
   Tail(Cons(Echar 'c', Cons(Echar 'd', Empty))), List [Char 'd'];
   Epair(Eint 2, Eint 3), Pair(Int 2, Int 3); 
   Epair(Echar 'c', Echar 'd'), Pair(Char 'c', Char 'd'); 
   Epair(True, False), Pair(Bool true, Bool false);
   Epair(Sum(Eint 2, Eint 3), Or(True, False)), Pair(Int 5, Bool true); 
   Fst(Epair(Eint 2, Eint 3)), Int 2; 
   Fst(Epair(Echar 'c', Echar 'd')), Char 'c'; 
   Fst(Epair(True, False)), Bool true; 
   Fst(Epair(Sum(Eint 2, Eint 3), Or(True, False))), Int 5;
   Snd(Epair(Eint 2, Eint 3)), Int 3; 
   Snd(Epair(Echar 'c', Echar 'd')), Char 'd'; 
   Snd(Epair(True, False)), Bool false; 
   Snd(Epair(Sum(Eint 2, Eint 3), Or(True, False))), Bool true;
   Ifthenelse(True, Sum(Eint 1, Eint 2), Diff(Eint 11, Eint 3)), Int 3;
   Ifthenelse(False, True, False), Bool false;
   Ifthenelse(True, Echar 'c', Echar 'd'), Char 'c';
   Let(Ide "x", Eint 2, Sum(Val(Ide "x"),Eint 3)), Int 5;
   Let(Ide "x", True, And(Val(Ide "x"), False)), Bool false;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), Char 'd';
   Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)), Appl(Val (Ide "x"), Eint 2)), Int 5;
(*   Fun(Ide "x", Val (Ide "x")), TFun(TVar "_", TVar "_");
   Fun(Ide "x", Sum(Val(Ide "x"), Eint 2)), TFun(TInt,TInt);
   Fun(Ide "x", And(Val(Ide "x"), True)), TFun(TBool,TBool);
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TFun(TChar,TChar);
   Fun(Ide "x", Appl(Fun(Ide "y", Val (Ide "y")), Val( Ide "x"))), TFun(TVar "_",TVar "_");
*)
   Appl(Fun(Ide "x", Val (Ide "x")), Eint 2), Int 2;
   Appl(Fun(Ide "x", Val (Ide "x")), True), Bool true;
   Appl(Fun(Ide "x", Val (Ide "x")), Echar 'c'), Char 'c'
(*
   Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))), TFun(TInt,TInt);
   Rec(Ide "y", (Fun(Ide "x", And(Val (Ide "x"), Appl(Val (Ide "y"), False))))), TFun(TBool,TBool)
*)
];;
	
let sbagliate = [
  Eq(Epair(Eint 2, Echar 'c'),Epair(Echar 'd', Eint 3));
  Eq(Cons(Eint 2, Empty), Cons(Echar 'c', Empty));
  Eq(Ifthenelse(True, Echar 'c', Echar 'd'), Ifthenelse(False, Eint 3, Eint 4));
  Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Echar 'c');
  Cons(Cons(Fun(Ide "x", And(Val(Ide "x"), True)), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty));
];;



(*---------------------------------------------------------------------------
Test per typeinf
-------------------------------------------------------------------------------*)
let isValid (index,expr) = match expr with 
      Int x -> (index+1, true)
    | Char x -> (index+1, true)
    | Bool x -> (index+1, true)
    | Closure(a,b) -> (index+1,true)
    | Pair(a,b) -> (index+1,true)
    | List _ -> (index+1,true)
    | _ -> (index,false);;




let testGiusteScoppio l =
let check l = 
  List.fold_left (fun acc x -> if snd acc 
                  then (try isValid(fst acc, semtry (fst x) emptyenv) with _ -> (fst acc,false))
                  else (fst acc,false)) (1,true) l
  in let result = check l
  in if snd result then "Tutto OK" else "Scoppio in "^ string_of_int (fst result);;




(* Ignora Fun, non essendo necessaria quell'uguaglianza *)
let testGiuste l =
  let rec testaGiustaExpr (expr,expected) = match expr, expected with
      (Int x, Int y) -> x = y
    | (Bool x, Bool y) -> x = y
    | (Char x,Char y) -> x = y
    | (Pair(a,b), Pair(c,d)) -> testaGiustaExpr (a,c) && testaGiustaExpr (b,d)
    | (Closure(a,c),b) -> true
    | (List [List a as c], List[List b as d]) -> testaGiustaExpr (c,d)
    | (List [], List []) -> true
    | (List a, List b) -> a = b
    | _ -> false
  in let check = List.fold_left (fun acc x -> 
                      if snd acc && testaGiustaExpr ((semtry (fst x) emptyenv),(snd x)) 
                      then ((fst acc)+1, true) else ((fst acc), false) )
       (1, true) l
  in if (snd check) then "Tutto OK" else  ("Errore in "^ string_of_int (fst check));;




let testSbagliate l = 
  let check l = List.fold_left 
    (fun acc x -> if not (snd acc) then 
       (try isValid(fst acc, semtry x emptyenv) with _ -> ((fst acc) + 1, false))
     else (fst acc, true)) (0,false) l
  in let result = check l
in if not (snd result) then "Tutto OK" else "La "^string_of_int (fst result)^" non scoppia come dovrebbe";;



(* AREA TEST *)

testGiusteScoppio giuste;; (* se va tutto bene, le espressioni "giuste" sono tutte inferibili *)

(* NON USARE se non si passa il test anti-scoppio *)
testGiuste giuste;;  (* verifica se il risultato di typeinf � quello atteso *)

testSbagliate sbagliate;; (* verifica che nessuna espressione "sbagliata" venga inferita come giusta *)
