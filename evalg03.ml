(************************************************************)
(*                  ENVIRONMENT/AMBIENTE                    *)
(************************************************************)

(* "tipo valutazione dell'esprezzione" *)
(* il tipo evn è da rivedere / chiedere al professore*)


type ide = Ide of string;;

type exp =
  Val of ide                     (*Valore *)
| Eint of int                    (*espressione intero*)
| Echar of char                  (*espressione carattere*)
| True                           (*Bool vero*)
| False                          (*Bool falso*)
| Empty                          (*lista vuota*)
| Sum of exp * exp               (* somma, appartiene agli operatori*)
| Diff of exp * exp              (* differenza, appartiene agli operatori*)
| Times of exp * exp             (* moltiplicazione, appartiene agli operatori*)
| And of exp * exp               (* operatore logico dell'and *)
| Or of exp * exp                (* operatore logico dell'or*)
| Not of exp                     (* negazione logica*)
| Eq of exp * exp                (*equivalenza*)
| Less of exp * exp              (*operatore logico minore*)
| Cons of exp * exp              (*costruttore della lista ::*)
| Head of exp                    (*testa di una lista*)
| Tail of exp                    (*Coda di una lista*)
| Fst of exp                     (* Primo elemento di una coppia*)
| Snd of exp                     (* secondo elemento di una coppia*)
| Epair of exp * exp              (*coppia *)
| Ifthenelse of exp * exp * exp  (*condizione?*)
| Let of ide * exp * exp         (*Inizio delimitatore Blocco?*)
| Fun of ide * exp               (*funzione*)
| Appl of exp * exp              (*applicazione*)
| Rec of ide * exp;;             (* funzione ricorsiva*)


type eval =
  Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env
and
 env = Env of (ide -> eval);;


exception UndefinedIde of ide;;
exception TypeMismatch ;;



let emptyenv =  Env(fun x -> Undefined)

and bind ((Env r), x, d) = Env (fun y -> if y=x then d else r y)

and 

applyenv ((Env r),x) = r x ;;




let rec evalInt e r = match (sem e r) with
  Int n -> n
| _ -> raise TypeMismatch
 
and evalBool e r = match sem e r with
  Bool b -> b
| _ -> raise TypeMismatch

and evalChar e r = match sem e r with
  Char c -> c
| _ -> raise TypeMismatch


(*vedere pagina 4 sul controllo delle variabili, serve a uscire dal loop?*)
(* espressione che produce, vecchio ambiente, nuovo ambiente*)

and controllerFV (e,d1,d2) = match e with
    Val v -> bind (d2, v, applyenv (d1,v)) 
  |Eint e1 -> d2
  |Echar e1 -> d2
  |True | False | Empty -> d2
  |Sum(e1,e2) |Diff(e1,e2) |Times(e1,e2) |And(e1,e2) |Or(e1,e2) 
  |Eq(e1,e2)  |Less(e1,e2) |Cons(e1,e2) |Epair(e1,e2) |Appl(e1,e2) 
      ->  controllerFV (e1,d1,(controllerFV (e2,d1,d2)))
  |Head e1 | Tail e1 | Fst e1 | Snd e1 | Not e1 -> controllerFV (e1,d1,d2) 
  |Ifthenelse (b,e1,e2) -> controllerFV (b,d1,(controllerFV (e1,d1,(controllerFV(e2,d1,d2)))))
  |Let (x, e1, e2) -> controllerFV (e1,d1,( controllerFV (e2,d1,(bind (d2, x, Undefined)))))
  |Rec (y, (Fun(x,t) as t1)) -> controllerFV (t1,d1,d2)
  |Fun (x, e1) -> controllerFV (e1,d1,(bind (d2,x,Undefined)))
  |_->failwith "controllo sul rec, manca il match completo"



(*ricorsione vedere pagina 4 prima delle regole della semantica *)
(* espressione che produce, vecchio ambiente, nuovo ambiente*)

and sub (e,oldV,newV)  = match e with
    Val i -> if i = oldV then newV else Val i
  | True | False | Empty -> e
  | Echar t -> e
  | Eint t -> e
  | Sum(e1,e2) -> Sum(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Diff(e1,e2) -> Diff(sub (e1,oldV,newV),sub (e2,oldV,newV))
  | Times(e1,e2) -> Times(sub  (e1,oldV,newV), sub (e2,oldV,newV))
  | And(e1,e2) -> And(sub  (e1,oldV,newV), sub (e2,oldV,newV))
  | Or(e1,e2) -> Or(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Eq(e1,e2) -> Eq(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Less(e1,e2) -> Less(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Cons(e1,e2) -> Cons(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Epair(e1,e2) -> Epair(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Not e1 -> Not(sub (e1,oldV,newV))
  | Head e1 -> Head(sub (e1,oldV,newV))
  | Tail e1 -> Tail(sub (e1,oldV,newV))
  | Fst e1 -> Fst(sub (e1,oldV,newV))
  | Snd e1 -> Snd(sub (e1,oldV,newV))
  | Ifthenelse(b,e1,e2) -> Ifthenelse(sub (b,oldV,newV), sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Let(x,e1,e2) -> Let(x, sub (e1,oldV,newV), sub (e2,oldV,newV))
  | Fun(x,e1) -> Fun(x, sub (e1,oldV,newV))
  | Appl(e1,e2) -> Appl(sub (e1,oldV,newV), sub (e2,oldV,newV))
  | _ -> failwith "Errore nella sostituzione Rec, manca il match completo?"

and sem e r = match e with
    Eint n -> Int n
  | Echar c -> Char c  
  | Val x ->  applyenv (r,x)
  | Sum   (e1,e2) -> Int (evalInt e1 r + evalInt e2 r) 
  | Diff  (e1,e2) -> Int (evalInt e1 r - evalInt e2 r)
  | Times (e1,e2) -> Int (evalInt e1 r * evalInt e2 r)
  | True  -> Bool true
  | False -> Bool false
  | Eq (e1,e2) -> (match sem e1 r, sem e2 r with
                       Int a, Int b   -> Bool (a=b) 
                     | Bool a, Bool b -> Bool (a=b) 
                     | Char a, Char b ->  Bool (a=b) 
                     | List a, List b -> Bool (a=b)
                     | Pair(a,b), Pair (c,d) -> Bool (a=c&&b=d)
                     | Closure(a,b), Closure (c,d) -> Bool (a=c&&b=d)    
                     |Undefined, Undefined -> Bool (Undefined=Undefined)
                     |_-> raise TypeMismatch)
  | Less (e1,e2) -> Bool (evalInt e1 r <= evalInt e2 r)
  | Not ne -> Bool (not (evalBool ne r))
  | And (e1,e2) -> Bool (evalBool e1 r && evalBool e2 r)
  | Or (e1,e2) -> Bool (evalBool e1 r || evalBool e2 r)
  | Empty -> List []
  |Head e1        -> let a = sem e1 r in 
                   let b = (match a with
                   List [] -> failwith "Lista vuota" 
                   |List (hd::tl) -> hd  
                   |_ -> raise TypeMismatch)         
                   in b 
  |Tail e1        -> let a = sem e1 r in 
                   let b = (match a with
                   List [] -> failwith "Lista vuota" 
                   |List (hd::tl) -> tl  
                   |_ -> raise TypeMismatch)         
                in List b 
  | Cons (e1, e2) ->(let a = sem e2 r in 
                   let b = (match a with                      
                   |(List l) ->(match l with
                               [] -> (sem e1 r)::[]  
                               |(hd::tl) -> (match sem e1 r , hd  with
                                            Int a,  Int  b -> (Int a)::l 
                                          | Bool a, Bool b -> (Bool a)::l
                                          | Char a, Char b -> (Char a)::l
                                          | Pair (a,b), Pair (c,d) -> (Pair (a,b))::l
                                          | Closure (a,b), Closure (c,d) -> (Closure (a,b))::l
                                          | List a,List b -> (List a)::l 
                                          | Undefined, Undefined -> (Undefined)::l
                                          |_ -> raise TypeMismatch))
                      |_ -> raise TypeMismatch)
                        
                   in  List b )
  |Epair (e1,e2) -> Pair ( sem e1 r, sem e2 r)
  |Fst e -> ( match (sem e r) with
           Pair (a, b) -> a
              |_-> raise TypeMismatch)
  |Snd e -> ( match (sem e r) with
           Pair (a, b) -> b
          |_-> raise TypeMismatch)
  | Ifthenelse(e0,e1,e2) -> if evalBool e0 r then sem e1 r else sem e2 r
  | Let (x,e1,e2) -> sem e2 (bind (r,x,(sem e1 r)))
  | Rec(y,(Fun(x,e1))) -> 
          let newValue = (sub (e1,y,(Rec(y,Fun(x,e1))))) in
            Closure(Fun(x,newValue), controllerFV (newValue,r,emptyenv))
  | Fun (x,e1) -> Closure ((Fun(x,e1)), controllerFV (e1,r,emptyenv))
  | Appl (e1,e2) -> (match sem e1 r with
                Closure ((Fun(x,f)),d) -> (sem f (bind (d,x,(sem e2 r))))  
               | _ -> failwith "coddati")
  |_-> failwith "sem rec madonna, matcha male come negli altri casi"

;;

let e0 = Let(
  Ide "succ",
  Fun(Ide "x", Sum(Val(Ide "x"), Eint 1)),
  Appl(Val(Ide "succ"),Eint 8));;

let a = Sum (Eint 2, Eint 3);;
sem e0 emptyenv;;


sem (Cons (Eint 2, Empty)) emptyenv;;



let e1 = Let(Ide "fact",Rec(Ide "x",
		Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"),Eint 0),Eint 1, Times(Val(Ide "x"),
			   Appl(Val(Ide "fact"),Diff(Val(Ide "x"), Eint 1)))))),
	       Appl(Val(Ide "fact"),Eint 5));;
sem e1 emptyenv;;



let e1 = Rec(Ide "y", Let(Ide "x",Fun(Ide "x", Sum(Val(Ide "x"), Eint 1)),Appl(Val(Ide "x"),Eint 8)));;
 
sem e1 emptyenv;;
 
 sem (Fun(Ide "x", Sum(Val(Ide "x"), Eint 1))) emptyenv;;
 sem(Times(Eint 4,Eint 5))  emptyenv;;
 sem(Eq(Eint 2,Eint 4))  emptyenv;;
 sem(Eq(Eint 2,Eint 2))  emptyenv;;
 sem(Times(Eint 3,Eint 4))  emptyenv;;
 sem(Sum(Eint 3,Eint 2))  emptyenv;;
 sem(Diff(Eint 5,Eint 3))  emptyenv;;
 sem(Diff(Eint 5,Eint 8)) emptyenv;;    
 sem(And(True,False))  emptyenv;;
 sem(And(True,True))  emptyenv;;
 sem(And(False,True))  emptyenv;;
 sem(And(False,False))  emptyenv;;
 sem(Or(True,False))  emptyenv;;
 sem(Or(True,True))  emptyenv;;
 sem(Or(False,True))  emptyenv;;
 sem(Or(False,False))  emptyenv;;     
 sem(Less(Eint 5,Eint 3))  emptyenv;;
 sem(Less(Eint 3,Eint 5))  emptyenv;;
 sem(Not(True))  emptyenv;;
 sem(Not(False))  emptyenv;;
 sem(True) emptyenv;;
 sem(False) emptyenv;;
 sem(Empty) emptyenv;;
 sem(Fst(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) ))) emptyenv;;
 sem(Snd(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) ))) emptyenv;;    
 sem(Fst(Sum(Eint 2, Eint 3))) emptyenv;; (* errore, ma non mi torna il test*)
 sem(Sum(Eint 2,Eint 3)) emptyenv;;     
 sem ((Cons(Eint 4,Cons(Eint 2,(Cons(Eint 1,Empty)))))) emptyenv;;
 sem ((Head(Cons(Eint 2,(Cons(Eint 1,Empty)))))) emptyenv;;
 sem ((Head(Cons(Eint 2,Empty)))) emptyenv;;
 sem(Head(Empty)) emptyenv;;
 sem ((Tail((Cons(Eint 3,Cons(Eint 2,(Cons(Eint 1,Empty)))))))) emptyenv;;   
 sem ((Tail(Cons(Eint 10,Empty)))) emptyenv;;
 sem (Tail(Empty)) emptyenv;;    

let a = (Fst(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) )));;
let b =(Snd(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) )));;


sem (Eq(a,b)) emptyenv;;

sem (Fst(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) ))) emptyenv;;
sem (Snd(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) ))) emptyenv;;



let e1 = Rec(Ide "fact",Let(Ide "fact",
		Fun(Ide "x", 
		    Ifthenelse(Eq(Val(Ide "x"),Eint 0),
		       Eint 1, 
		       Times(Val(Ide "x"),
			   Appl(Val(Ide "fact"),Diff(Val(Ide "x"), Eint 1))))),
	       Appl(Val(Ide "fact"),Eint 5)));;

sem e1 emptyenv;;


let a =emptyenv;;

let b = bind (a,(Ide "x"), Int 2);;

applyenv (b,(Ide "x"));;



let e1 = Let(Ide "fact",
             Rec(Ide "fact", 
		Fun(Ide "x",  Ifthenelse(
                      Eq(Val(Ide "x"),Eint 0), 
                      Eint 1, 
                      Times(Val(Ide "x"), Appl(Val(Ide "fact"),Diff(Val(Ide "x"),Eint 1)))
                    ))),
	       Appl(Val(Ide "fact"),Eint 5));;
 
sem e1 emptyenv;;

let e0 =Rec (Ide "y", 
                Fun (Ide "y", 
                   Ifthenelse(  Eq(Val(Ide "y"),Eint 0), 
                                         Eint 1, 
                                         Times (Val (Ide "y"), Appl (Val (Ide "y"), Eint 2)))));;


sem e0 emptyenv;;


let e0 =Let (Ide "prova", 
             Rec (Ide "y", 
                Fun (Ide "y", 
                   Ifthenelse(  Eq(Val(Ide "y"),Eint 0), 
                                         Eint 1, 
                                         Times (Val (Ide "y"), Appl (Val (Ide "y"), Eint 2))))), 
             Appl(Val (Ide "prova"), Val (Ide "y")));;

let e1 = Appl( Fun ( Ide "x", Sum (Val (Ide "x"), Eint 3)), Eint 5);; 
sem e1 emptyenv;;

let e3 = Appl(Rec (
              (Ide "y"),
               Appl( Fun ((Ide "x"), 
                          Ifthenelse(  Eq(Val(Ide "y"), Eint 0),  
                          Sum (Val (Ide "x"), Eint 1),                                             
                          Diff((Appl(Val (Ide "y"), Eint 3)), Eint 1))), 
               Eint 1)), Eint 2);;

sem e3 emptyenv;;

let recurs = Rec(Ide "y", Fun( Ide "x", 
                               Ifthenelse( (Eq(Val (Ide "x"), Eint 0) ),
                               (Eint 1),
                                           Sum( Eint 2, 
                                                Appl( Val (Ide "y"), Diff(Val (Ide "x"), Eint 1) )
                                              )
                                         )));;
sem recurs emptyenv;;
sem (Appl(recurs, Eint 2)) emptyenv;;
