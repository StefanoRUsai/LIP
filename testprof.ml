#use "evalg03.ml";;

typeinf (Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
     Appl(Appl(Val(Ide "f"),Eint 2),Eint 1)))  ;;

sem (Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
     Appl(Appl(Val(Ide "f"),Eint 2),Eint 1))) emptyenv;;

(*****)

typeinf (Let(Ide "fact",Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
    Eq(Val(Ide "x"), Eint 0), Eint 1,
	Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"), Eint 1)))))),
	Appl(Val(Ide "fact"),Eint 5)))  ;;

sem  (Let(Ide "fact",Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
    Eq(Val(Ide "x"), Eint 0), Eint 1,
	Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"), Eint 1)))))),
	Appl(Val(Ide "fact"),Eint 5))) emptyenv;;


(*****)

typeinf (Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
    Eq(Val(Ide "x"), Eint 0), Eint 1,
	Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"), Eint 1))))))) ;;

sem  (Rec(Ide "fact", Fun(Ide "x", Ifthenelse(
    Eq(Val(Ide "x"), Eint 0), Eint 1,
	Times(Val(Ide "x"), Appl (Val(Ide "fact"), Diff(Val(Ide "x"), Eint 1))))))) emptyenv;;

(*****)

typeinf ((Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
     Appl(Appl(Val(Ide "f"),Eint 3),Echar 'c'))))  ;;

sem  (Let(Ide "f", Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))),
     Appl(Appl(Val(Ide "f"),Eint 3),Echar 'c'))) emptyenv;;


(*****)

typeinf (Fun(Ide "x",Fun(Ide "y",Val(Ide "x"))));;

sem  (Fun(Ide "x",Fun(Ide "y",Val(Ide "x")))) emptyenv;;


(*****)

typeinf (Cons(Empty,Empty)) ;;

sem  (Cons(Empty,Empty)) emptyenv;;

(*****)

typeinf (Cons(Eint 1, Cons(Eint 2, Empty)));;

sem  (Cons(Eint 1, Cons(Eint 2, Empty))) emptyenv;;


(*****)

typeinf (Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2, Empty))) ) ;;

sem  (Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2, Empty))) ) emptyenv;;

(*****)

typeinf (Eq(Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2, Empty))),False));;

sem  (Eq(Eq(Cons(Eint 1, Cons(Eint 2, Empty)),Cons(Eint 1, Cons(Eint 2, Empty))),False)) emptyenv;;

(*****)

typeinf (Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)));;

sem  (Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False))) emptyenv;;



(*****)

typeinf (Let(Ide "f",Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),Appl(Val(Ide "f"),Cons(Eint 2, Empty)))) ;;


sem  (Let(Ide "f",Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
             Appl(Val(Ide "f"),Cons(Eint 2, Empty)))) emptyenv;;
(*****!!!!!!!!!!********)



(*****)

typeinf (Cons(Cons(Eint 1,Empty),Empty))  ;;
sem  (Cons(Cons(Eint 1,Empty),Empty)) emptyenv;;



(*****)

typeinf (Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),Cons(Cons(Eint 1,Empty),Empty))) ;;
sem (Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),Cons(Cons(Eint 1,Empty),Empty))) emptyenv;;

(*****)

typeinf (Appl(
   Fst(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
                                     Cons(Cons(Eint 1,Empty),Empty))),
       Snd(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
                                     Cons(Cons(Eint 1,Empty),Empty)))))  ;;

sem  (Appl(
   Fst(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
                                     Cons(Cons(Eint 1,Empty),Empty))),
       Snd(Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),
                                     Cons(Cons(Eint 1,Empty),Empty))))
) emptyenv;;


(*******************************!!!!!!!!******)




(*****)

typeinf  (Let(Ide "p",Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),Cons(Cons(Eint 1,Empty),Empty)),Appl(Fst(Val(Ide "p")),Snd(Val(Ide "p")))));;

sem (Let(Ide "p",Epair(Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Empty),True,False)),Cons(Cons(Eint 1,Empty),Empty)),Appl(Fst(Val(Ide "p")),Snd(Val(Ide "p")))))
 emptyenv;;


(*NON DEVE FARLA**)
typeinf (Cons(Eint 1,Cons(True, Empty)));;

sem (Cons(Eint 1,Cons(True, Empty))) emptyenv;;



(*NON DEVE FARLA***)

typeinf (Eq(Cons(Eint 1,Empty),Cons(True,Empty)))  ;;

sem (Eq(Cons(Eint 1,Empty),Cons(True,Empty))) emptyenv;;



(****)

typeinf (Let(Ide "f",Rec(Ide "f",Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Eint 0),Empty,
            Cons(Val(Ide "x"),Appl(Val(Ide "f"),Diff(Val(Ide "x"),Eint 1)))))),
            Appl(Val(Ide "f"),Eint 5)))  ;;

sem (Let(Ide "f",Rec(Ide "f",Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Eint 0),Empty,
            Cons(Val(Ide "x"),Appl(Val(Ide "f"),Diff(Val(Ide "x"),Eint 1)))))),
            Appl(Val(Ide "f"),Eint 5))
) emptyenv;;


(********************* !!!!!!!!!!! **********)








