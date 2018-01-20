#use "typingg03.ml";;

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
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 2, Echar 'c')), TBool;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 3214, Echar 'd')), TBool;
   Eq(Cons(Eint 3, Empty), Cons(Eint 3, Empty)), TBool;
   Eq(Cons(Eint 2, Empty), Cons(Eint 3, Empty)), TBool;
   Eq(Let(Ide "x", Echar 'c', Val (Ide "x")), Echar 'c'), TBool; 
   Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Eint 3), TBool;
   Eq(Ifthenelse(True, Eint 2, Eint 3), Ifthenelse(False, Eint 3, Eint 4)), TBool;
   Eq(Ifthenelse(True, Eint 1, Eint 3), Ifthenelse(False, Eint 3, Eint 5)), TBool;
   Eq(Ifthenelse(Eq(Sum(Eint 1,Eint 0),Eint 0),Less(Eint 2, Eint 5),And(True,Not(Not(True)))),Ifthenelse(Not(And(True,And(Or(False,False),True))),Head(Fst(Epair(Cons(True,Empty),Echar 'v'))), False)  ),TBool;
   Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Appl(Rec(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 2))), Eint 2)), TBool;
   Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Eint 3), TBool;
   Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Appl(Rec(Ide "x", Fun(Ide "y", Val(Ide "y"))), Eint 2)), TBool;
   Eq(Snd(Epair(Echar 'c',Tail(Cons(Eint 5,Cons(Eint 2,Empty))))), Cons(Eint 3, Empty)),TBool;
   Cons(Eint 3, Empty), TList[TInt]; 
   Cons(Echar 'c', Empty), TList[TChar]; 
   Cons(True, Empty), TList[TBool]; 
   Cons(Eint 2, Cons(Eint 3, Empty)), TList[TInt];
   Cons(True, Cons(False, Empty)), TList[TBool];
   Cons(Echar 'c', Cons(Echar 'd', Empty)), TList[TChar];
   Cons(Cons(Eint 2, Empty), Cons(Cons(Eint 3, Empty), Empty)), TList[TList[TInt]];
   Cons(Cons(Let(Ide "x", Echar 'c', Val (Ide "x")), Empty), Cons(Empty, Empty)), TList[TList[TChar]];
   Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty)), TList[TFun(TInt, TInt)];
   Cons(Cons(Eq(Echar 'c', Echar 'q'), Empty), Cons(Cons(Ifthenelse(Eq(Eint 1,Eint 1),Eq(Echar 't',Echar 'f'),Eq(Echar 't',Echar 't')), Empty), Empty)),TList [TList [TBool]];
   Head(Empty), TVar "_";
   Head(Cons(Eint 3, Empty)), TInt; 
   Head(Cons(Echar 'c', Empty)), TChar;
   Head(Cons(True, Empty)), TBool; 
   Head(Cons( Eint 2, Cons(Eint 3, Empty))), TInt;
   Head(Cons(True, Cons(False, Empty))), TBool; 
   Head(Cons(Echar 'c', Cons(Echar 'd', Empty))), TChar;
   Head(Cons(True, Cons(False, Empty))), TBool;
   Head(Tail(Cons(Eint 4,Cons(Snd(Epair(Echar 'v',Sum(Times(Ifthenelse(Eq(Eint 2,Eint 2),Eint 5,Eint 1),Eint 1),Eint 1))),Empty)))),TInt;
   Tail(Empty), TList[TVar "_"];
   Tail(Cons(Eint 3, Empty)), TList[TInt]; 
   Tail(Cons(Echar 'c', Empty)), TList[TChar];
   Tail(Cons(True, Empty)), TList[TBool]; 
   Tail(Cons( Eint 2, Cons(Eint 3, Empty))), TList[TInt];
   Tail(Cons(True, Cons(False, Empty))),TList[TBool];
   Tail(Cons(Echar 'c', Cons(Echar 'd', Empty))), TList[TChar];
   Tail(Snd(Epair(Eint 4,Cons(Eint 2,Cons(Eint 4,Cons(Fst(Epair(Sum(Eint 2,Eint 5),Times(Eint 5,Eint 8))),Empty)))))),  TList [TInt];
   Epair(Eint 2, Eint 3), TPair(TInt, TInt); 
   Epair(Echar 'c', Echar 'd'), TPair(TChar, TChar); 
   Epair(True, False), TPair(TBool, TBool);
   Epair(Sum(Eint 2, Eint 3), Or(True, False)), TPair(TInt, TBool); 
   Epair(Less(Eint 5,Eint 1),Ifthenelse(Eq(Sum(Eint 3,Eint 1),Eint 4),Snd(Epair(Eint 3,Echar 'v')),Echar 'c')), TPair(TBool, TChar); 
   Epair(Cons(True,Cons(Not(False),Cons(False,Empty))), Fst(Epair(Echar 'w',Sum(Eint 3,Eint 7)))),TPair (TList [TBool], TChar);
   Epair(Sum(Eint(-5),Head(Tail(Cons(Eint 5,Cons(Eint 4,Empty))))),Ifthenelse(Not(Eq(Less(Sum(Times(Eint 1,Eint 0),Eint 2),Eint 0),Less(Diff(Fst(Epair(Eint 1,Echar 'a')),Eint 4),Eint 1))),Not(Less(Head(Cons(Eint 2,Cons(Eint 3,Empty))),Eint 3)),And(True,Less(Head(Tail(Cons(Eint 2,Cons(Eint 3,Empty)))),Eint 4)))),TPair (TInt, TBool);
   Epair(Appl(Fun(Ide "x", Val (Ide "x")),Snd(Epair(Sum(Eint 5,Eint 1),Times(Head(Cons(Eint 1,Cons(Eint 2,Empty))),Head(Fst(Epair(Cons(Eint 2,Cons(Sum(Eint 1,Eint 0),Empty)),Not(Or(True,And(False,False)))))))))),Ifthenelse(Not(Not(Not(Ifthenelse(Eq(True,True),Less(Eint 1,Eint 0),And(Or(True,True),False))))),Not(And(True,True)),And(False,True))),TPair (TInt, TBool);
   Fst(Epair(Eint 2, Eint 3)), TInt; 
   Fst(Epair(Echar 'c', Echar 'd')), TChar; 
   Fst(Epair(True, False)), TBool; 
   Fst(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TInt;
   Snd(Epair(Eint 2, Eint 3)), TInt; 
   Snd(Epair(Echar 'c', Echar 'd')),TChar; 
   Snd(Epair(True, False)),TBool; 
   Snd(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TBool;
   Snd(Epair(True,Cons(Cons(Cons(Cons(True,Empty),Empty),Cons(Empty,Empty)),Empty))), TList [TList [TList [TList [TBool]]]];
   Snd(Head(Cons(Epair(Echar 'c',Eq(Eint 2,Eint 4)),Cons(Epair(Echar 'f',Less(Eint 2,Eint 4)),Empty)))),TBool;
   Ifthenelse(True, Sum(Eint 1, Eint 2), Diff(Eint 11, Eint 3)), TInt;
   Ifthenelse(False, True, False), TBool;
   Ifthenelse(True, Echar 'c', Echar 'd'), TChar;
   Ifthenelse(Eq(Sum(Eint 3,Eint 1),Diff(Eint 5,Eint 1)),Times(Eint 2,Eint 5),Snd(Epair(Echar 'a',Eint 7))), TInt;
   Ifthenelse(Eq(Fst(Epair(Eint 5,Echar 'v')),Fst(Epair(Eint 5,Echar 'w'))),Tail(Cons(Echar 'v',Cons(Echar 'q',Cons(Echar 'w',Empty)))),Cons(Head(Cons(Echar 'r',Cons(Echar 'm',Empty))),Cons(Echar 'q',Empty))  ),TList [TChar];
   Ifthenelse(Eq(Cons(Echar 'a',Cons(Echar 'b',Empty)),Cons(Echar 'a',Cons(Echar 'c',Empty))),Head(Tail((Cons(Echar 'a',Cons(Echar 'b',Empty))))),Fst(Epair(Snd(Epair(Eint 1,Echar 'c')),True))),TChar;
   Let(Ide "x", Eint 2, Sum(Val(Ide "x"),Eint 3)), TInt;
   Let(Ide "x", True, And(Val(Ide "x"), False)), TBool;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TChar;
   Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)),Appl(Val(Ide "x"), Eint 2)), TInt; 
   Let(Ide "x", Echar 'v', Eq(Val(Ide "x"),Echar 'z')), TBool;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Sum(Eint 1,Eint 2),Diff(Eint 2,Eint 1))), TInt;
   Let(Ide "x", Ifthenelse(Eq(Or(True,False),True),Echar 'c',Echar 'v'),Ifthenelse(Eq(Val(Ide "x"),Echar 'c'),Echar 'd',Echar 'f')),TChar;
   Fun(Ide "x", Val (Ide "x")), TFun(TVar "_", TVar "_");
   Fun(Ide "x", Sum(Val(Ide "x"), Eint 2)), TFun(TInt,TInt);
   Fun(Ide "x", And(Val(Ide "x"), True)), TFun(TBool,TBool);
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TFun(TChar,TChar);
   Fun(Ide "x", Appl(Fun(Ide "y", Val (Ide "y")), Val( Ide "x"))), TFun(TVar "_",TVar "_");
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Head(Cons(Echar 'v',Cons(Echar 'a',Empty)))),True,False)), TFun(TChar,TBool); 
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Eint 3),Sum(Head(Cons(Eint 4,Empty)),Eint 2), Diff(Eint 3,Eint 1))) , TFun (TInt, TInt);
   Fun(Ide "x", Times(Eint 3,Sum(Val(Ide "x"),Ifthenelse(Less(Eint 1,Eint 4),Sum(Eint 0,Eint 1),Sum(Eint 6,Diff(Eint 3,Eint 1)))))),TFun (TInt, TInt);
   Appl(Fun(Ide "x", Val (Ide "x")), Eint 2), TInt;
   Appl(Fun(Ide "x", Val (Ide "x")), True), TBool;
   Appl(Fun(Ide "x", Val (Ide "x")), Echar 'c'), TChar;
   Appl(Fun(Ide "x", Val (Ide "x")), Snd(Epair(Sum(Eint 5,Eint 1),Times(Head(Cons(Eint 1,Cons(Eint 2,Empty))),Eint 2)))),TInt; 
   Appl(Fun(Ide "x", Val (Ide "x")), Ifthenelse(Eq(Sum(Eint 1,Eint 1),Head(Cons(Eint 1,Empty))),Let(Ide "x",Eint 2,Sum(Val(Ide "x"),Eint 3)),Sum(Eint 2,Fst(Epair(Eint 1,Eint 5))))),TInt;
   Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))), TFun(TInt,TInt);
   Rec(Ide "y", (Fun(Ide "x", And(Val (Ide "x"), Appl(Val (Ide "y"), False))))), TFun(TBool,TBool)
];;
	




let sbagliate = [
  Sum(True,False);
  Sum(Echar 'c', Echar 'd');
  Sum(Eint 3, True);
  Sum(Echar 'c', True);
  Times(True,False);
  Times(Echar 'c', Echar 'd');
  Times(Eint 3, True);
  Times(Echar 'c', True);
  Diff(True,False);
  Diff(Echar 'c', Echar 'd');
  Diff(Eint 3, True);
  Diff(Echar 'c', True);
  And(True, Eint 3);
  And(Eint 3, Eint 3);
  And(Echar 'c', Echar 'd');
  And(Eint 3, Echar 'c');
  And(True, Echar 'c');
  Or(True, Eint 3);
  Or(Eint 3, Eint 3);
  Or(Echar 'c', Echar 'd');
  Or(Eint 3, Echar 'c');
  Or(True, Echar 'c');
  Not(Eint 2);
  Not(Echar 'c');
  Less(True, Eint 2);
  Less(True, Echar 'c');
  Less(Eint 2, Echar 'c');
  Less(Echar 'c', Echar 'd');
  Less(True, False);
  Eq(True, Eint 2);
  Eq(False, Echar 'c');
  Eq(Eint 2, Echar 'c');
  Eq(Epair(Eint 2, Echar 'c'),Epair(Echar 'd', Eint 3));
  Eq(Cons(Eint 2, Empty), Cons(Echar 'c', Empty));
  Eq(Ifthenelse(True, Echar 'c', Echar 'd'), Ifthenelse(False, Eint 3, Eint 4));
  Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Echar 'c');
  Cons(Empty, Cons(Eint 2, Empty)); 
  Cons(Eint 2, Cons(Echar 'c', Empty));
  Cons(True, Cons(Echar 'd', Empty));
  Cons(True, Cons(Eint 2, Empty));    
  Cons(Cons(Fun(Ide "x", And(Val(Ide "x"), True)), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty));
  Head(Eint 2);
  Head(True);
  Head(Echar 'c');
  Tail(Eint 2);
  Tail(True);
  Tail(Echar 'c');
(* Ha senso testare Epair? *)
  Fst(Eint 2);
  Fst(True);
  Fst(Echar 'c');
  Fst(Cons(Eint 2, Cons(Eint 1, Empty)));
  Fst(Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)),Appl(Val(Ide "x"), Eint 2)) );
  Snd(Eint 2);
  Snd(True);
  Snd(Echar 'c');
  Snd(Cons(Eint 2, Cons(Eint 1, Empty)));
  Snd(Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)),Appl(Val(Ide "x"), Eint 2)) );


  

  Appl(Eint 2, Eint 3);
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
    (fun acc x ->
       if (snd acc) then 
         (try (fst (isValid(fst acc, typeinf x)), 
               not(snd (isValid(fst acc, typeinf x)) ))
          with _ -> ((fst acc) + 1, true))
       else acc) (0, true) l
  in let result = check l
in if (snd result) then "Tutto OK" else "La "^string_of_int (fst result)^" non scoppia come dovrebbe";;



(* AREA TEST *)

testGiusteScoppio giuste;; (* se va tutto bene, le espressioni "giuste" sono tutte inferibili *)

(* NON USARE se non si passa il test anti-scoppio *)
testGiuste giuste;;  (* verifica se il risultato di typeinf è quello atteso *)

testSbagliate sbagliate;; (* verifica che nessuna espressione "sbagliata" venga inferita come giusta *)

