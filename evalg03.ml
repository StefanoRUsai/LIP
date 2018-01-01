(*#use typingg03.ml;;*)


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
(*applyenv ((Env r),x) = match r x with
  Undefined -> raise (UndefinedIde  x)
  | _ as d -> d;; *) (*vuol dire che ogni eval diverso da  undefined si chiama  d e restituisce d*)


(* exp vecchio_ambiente nuovo_ambiente *)
let rec controllerFV (e,d1,d2) = match e with
    Val v -> bind (d2, v, applyenv (d1,v)) 
  |Eint e1 -> d2
  |Echar e1 -> d2
  |True | False | Empty -> d2
  |Sum(e1,e2) |Diff(e1,e2) |Times(e1,e2) |And(e1,e2) |Or(e1,e2) 
  |Eq(e1,e2)  |Less(e1,e2) |Cons(e1,e2) |Epair(e1,e2) |Appl(e1,e2) 
      ->  controllerFV (e1,d1,(controllerFV (e2,d1,d2)))
  |Head e1 | Tail e1 | Fst e1 | Snd e1 | Not e1 -> controllerFV (e1,d1,d2) 
  |Ifthenelse (b,e1,e2) -> controllerFV (b,d1,(controllerFV (e1,d1,(controllerFV(e2,d1,d2)))))
  |Fun (x, e1)|Rec (x, e1) -> controllerFV (e1,d1,(bind (d2,x,Undefined)))
  |Let (x, e1, e2) -> controllerFV (e1,d1,( controllerFV (e2,d1,(bind (d2, x, Undefined)))));;


let rec evalInt e r = match (sem e r) with
  Int n -> n
| _ -> raise TypeMismatch
 
and evalBool e r = match sem e r with
  Bool b -> b
| _ -> raise TypeMismatch

and evalChar e r = match sem e r with
  Char c -> c
| _ -> raise TypeMismatch


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
(*  | Rec (x,e1) -> let rec r1 = Env(fun y -> applyenv ((bind (r,x,(sem e1 r1))),y)) in sem e1 r1*)
  | Rec (y,e1) -> (let x,t = (match e1 with
                            Fun (i,e2) -> i,e2
                            |_-> failwith "non esiste funzione") in  
                  let e3,d= (match (sem (Fun (x,t)) r) with
                            Closure (e4,d1) ->e4,d1
                           |_ ->failwith "non esiste closure") in    
                  let  r1 = bind (d,y,(sem (Fun (x,t)) d)) in sem e1 r1) (* sarebbe bello se ci fosse la fun dopo il rec*)
  | Fun (x,e1) -> Closure ((Fun(x,e1)), controllerFV (e1,r,emptyenv))
  | Appl (e1,e2) -> (match sem e1 r with
                Closure ((Fun(x,f)),d) -> (sem f (bind (d,x,(sem e2 r))))
               | _ -> failwith "coddati")
;;

let e0 = Let(
  Ide "succ",
  Fun(Ide "x", Sum(Val(Ide "x"), Eint 1)),
  Appl(Val(Ide "succ"),Eint 8));;

let a = Sum (Eint 2, Eint 3);;
sem e0 emptyenv;;






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
