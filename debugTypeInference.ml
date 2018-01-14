
let filename = "typing03.ml";;
#use "typing03.ml";;

(* Vari tests *)
let giuste = 
  [Eint 2, TInt; 
   Echar 'c', TChar; 
   True, TBool; 
   False, TBool; 
   Empty, TVar "_";
   Sum(Eint 2, Eint 3),TInt;
   Diff(Eint 2, Eint 3), TInt;
   Times(Eint 2, Eint 3), TInt; 
   And(True, False), TBool;
   Or(False, True), TBool; 
   Not(True), TBool;
   Less(Eint 2, Eint 3), TBool; 
   Less(Sum(Eint 2, Eint 3), Eint 3), TBool; 
   Less(Diff(Eint 2, Eint 3), Eint 6), TBool;
   Eq(Eint 2, Eint 4), TBool; 
   Eq(Eint 3, Eint 3), TBool; 
   Eq(Echar 'c', Echar 'd'), TBool; 
   Eq(True, False), TBool;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 3214, Echar 'd')), TBool;
   Eq(Cons(Eint 2, Empty), Cons(Eint 3, Empty)), TBool;
   Eq(Let(Ide "x", Echar 'c', Val (Ide "x")), Echar 'c'), TBool; 
   Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Eint 3), TBool;
   Eq(Ifthenelse(True, Eint 2, Eint 3), Ifthenelse(False, Eint 3, Eint 4)), TBool;
   Cons(Eint 3, Empty), TList[TInt]; 
   Cons(Echar 'c', Empty), TList[TChar]; 
   Cons(True, Empty), TList[TBool]; 
   Cons( Eint 2, Cons(Eint 3, Empty)), TList[TInt];
   Cons(True, Cons(False, Empty)), TList[TBool]; Cons(Echar 'c', Cons(Echar 'd', Empty)), TList[TChar];
   Cons(Cons(Eint 2, Empty), Cons(Cons(Eint 3, Empty), Empty)), TList[TList[TInt]];
   Cons(Cons(Let(Ide "x", Echar 'c', Val (Ide "x")), Empty), Cons(Empty, Empty)), TList[TList[TChar]];
   Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty)), TList[TFun(TInt, TInt)];
   Head(Empty), TVar "_"; 
   Head(Cons(Eint 3, Empty)), TInt; 
   Head(Cons(Echar 'c', Empty)), TChar;
   Head(Cons(True, Empty)), TBool; 
   Head(Cons( Eint 2, Cons(Eint 3, Empty))), TInt;
   Head(Cons(True, Cons(False, Empty))), TBool; 
   Head(Cons(Echar 'c', Cons(Echar 'd', Empty))), TChar;
   Tail(Empty), TList[TVar "_"]; 
   Tail(Cons(Eint 3, Empty)), TList[TInt]; 
   Tail(Cons(Echar 'c', Empty)), TList[TChar];
   Tail(Cons(True, Empty)), TList[TBool]; 
   Tail(Cons( Eint 2, Cons(Eint 3, Empty))), TList[TInt];
   Tail(Cons(True, Cons(False, Empty))),TList[TBool];
   Tail(Cons(Echar 'c', Cons(Echar 'd', Empty))), TList[TChar];
   Epair(Eint 2, Eint 3), TPair(TInt, TInt); 
   Epair(Echar 'c', Echar 'd'), TPair(TChar, TChar); 
   Epair(True, False), TPair(TBool, TBool);
   Epair(Sum(Eint 2, Eint 3), Or(True, False)), TPair(TInt, TBool); 
   Fst(Epair(Eint 2, Eint 3)), TInt; 
   Fst(Epair(Echar 'c', Echar 'd')), TChar; 
   Fst(Epair(True, False)), TBool; 
   Fst(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TInt;
   Snd(Epair(Eint 2, Eint 3)), TInt; 
   Snd(Epair(Echar 'c', Echar 'd')),TChar; 
   Snd(Epair(True, False)),TBool; 
   Snd(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TBool;
   Ifthenelse(True, Sum(Eint 1, Eint 2), Diff(Eint 11, Eint 3)), TInt;
   Ifthenelse(False, True, False), TBool;
   Ifthenelse(True, Echar 'c', Echar 'd'), TChar;
   Let(Ide "x", Eint 2, Sum(Val(Ide "x"),Eint 3)), TInt;
   Let(Ide "x", True, And(Val(Ide "x"), False)), TBool;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TChar;
   Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)), Appl(Val (Ide "x"), Eint 2)), TInt;
   Fun(Ide "x", Val (Ide "x")), TFun(TVar "_", TVar "_");
   Fun(Ide "x", Sum(Val(Ide "x"), Eint 2)), TFun(TInt,TInt);
   Fun(Ide "x", And(Val(Ide "x"), True)), TFun(TBool,TBool);
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TFun(TChar,TChar);
   Fun(Ide "x", Appl(Fun(Ide "y", Val (Ide "y")), Val( Ide "x"))), TFun(TVar "_",TVar "_");
   Appl(Fun(Ide "x", Val (Ide "x")), Eint 2), TInt;
   Appl(Fun(Ide "x", Val (Ide "x")), True), TBool;
   Appl(Fun(Ide "x", Val (Ide "x")), Echar 'c'), TChar;
   Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))), TFun(TInt,TInt);
   Rec(Ide "y", (Fun(Ide "x", And(Val (Ide "x"), Appl(Val (Ide "y"), False))))), TFun(TBool,TBool)
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
      TInt | TChar | TBool -> (index+1, true)
    | TVar a -> (index +1,true)
    | TFun(a,b) -> (index+1,true)
    | TPair(a,b) -> (index+1,true)
    | TList [a] -> (index+1,true)
    | _ -> (index,false);;

let testGiusteScoppio l =
let check l = 
  List.fold_left (fun acc x -> if snd acc 
                  then (try isValid(fst acc, typeinf (fst x)) with _ -> (fst acc,false))
                  else (fst acc,false)) (1,true) l
  in let result = check l
  in if snd result then "Tutto OK" else "Scoppio in "^ string_of_int (fst result);;


let testGiuste l = 
  let rec testaGiustaExpr (expr,expected) = match expr, expected with
      (TInt,TInt) | (TBool, TBool) | (TChar,TChar) -> true
    | (TVar n1, TVar "_") -> true
    | (TPair(a,b),TPair(c,d)) -> testaGiustaExpr (a,c) && testaGiustaExpr (b,d)
    | (TFun(a,b), TFun(c,d)) -> testaGiustaExpr (a,c) && testaGiustaExpr (b,d)
    | (TList [a], TList [b]) -> testaGiustaExpr (a,b)
    | (TList [a], b) -> testaGiustaExpr (a,b)
    | _ -> false
  in let check = List.fold_left (fun acc x -> 
                      if snd acc && testaGiustaExpr ((typeinf (fst x)),(snd x)) 
                      then ((fst acc)+1, true) else ((fst acc), false) )
       (1, true) l
  in if (snd check) then "Tutto OK" else  ("Errore in "^ string_of_int (fst check));;




let testSbagliate l = 
let check l = List.fold_left 
  (fun acc x -> if not (snd acc) then 
     (try isValid(fst acc, typeinf x) with _ -> ((fst acc) + 1, false))
   else (fst acc, true)) (0,false) l
in let result = check l
in if not (snd result) then "Tutto OK" else "La "^string_of_int (fst result)^" non scoppia come dovrebbe";;


(* AREA TEST *)

testGiusteScoppio giuste;; (* se va tutto bene, le espressioni "giuste" sono tutte inferibili *)

(* NON USARE se non si passa il test anti-scoppio *)
testGiuste giuste;;  (* verifica se il risultato di typeinf è quello atteso *)

testSbagliate sbagliate;; (* verifica che nessuna espressione "sbagliata" venga inferita come giusta *)












