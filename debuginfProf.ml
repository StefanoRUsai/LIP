#use "typingg03.ml";;

(* Vari tests *)
let giuste = 
  [Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
       Appl(Appl(Val(Ide "f"),Eint 2),Eint 1)), TInt; 

   Let(Ide "fact",Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
                                        Eq(Val(Ide "x"), Eint 0), Eint 1,
                                        Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"),
                                                                                        Eint 1)))))),
       Appl(Val(Ide "fact"),Eint 5)), TInt; 

   Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
                         Eq(Val(Ide "x"), Eint 0), Eint 1,
                         Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"),
                                                                         Eint 1)))))), TFun (TInt, TInt);
 
   Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
       Appl(Appl(Val(Ide "f"),Eint 3),Echar 'c')), TInt;
 
   Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))), TFun (TVar "_", TFun (TVar "_", TVar "_"));

   Cons(Empty,Empty), TList [TList [TVar "_"]];

   Cons(Eint 1, Cons(Eint 2, Empty)), TList [TInt];

   Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2,
                                                          Empty))), TBool; 

   Eq(Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2,
                                                             Empty))),False), TBool;


   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)), TFun (TList [TVar "_"], TBool); 

   Let(Ide "f",Fun(Ide "x", Ifthenelse(Eq(Val(Ide
                                                "x"),Empty),True,False)),Appl(Val(Ide "f"),Cons(Eint 2, Empty))), TBool;

   Cons(Cons(Eint 1,Empty),Empty), TList [TList [TInt]]; 

   Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide
                                          "x"),Empty),True,False)),Cons(Cons(Eint 1,Empty),Empty)), 
   TPair (TFun (TList [TVar "_"], TBool), TList [TList[TInt]]);

 
   Appl(
     Fst(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide
                                                "x"),Empty),True,False)),
               Cons(Cons(Eint 1,Empty),Empty))),
     Snd(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide
                                                "x"),Empty),True,False)),
               Cons(Cons(Eint 1,Empty),Empty)))), TBool;
   

   Let(Ide "p",Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide
                                                      "x"),Empty),True,False)),Cons(Cons(Eint
                                                                                           1,Empty),Empty)),Appl(Fst(Val(Ide "p")),Snd(Val(Ide "p")))), TBool; 
   
   Let(Ide "f",Rec(Ide "f",Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Eint
                                                       0),Empty,
                                                  Cons(Val(Ide "x"),Appl(Val(Ide "f"),Diff(Val(Ide "x"),Eint
                                                                                             1)))))),
       Appl(Val(Ide "f"),Eint 5)), TList [TInt]; 


  
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


