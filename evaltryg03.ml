
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
| Rec of ide * exp               (* funzione ricorsiva*)
| Try of exp*ide*exp
| Raise of ide;;

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



let trasforma (Ide a) = (a:string);;


let rec emptyenv =  Env(fun x -> Undefined)
and bind (r, x, d) = Env (fun y -> if y=x then d else applyenv  (r,y))
and applyenv ((Env r),x) = r x ;;


let rec evalInt e r = match semtry e r with
  Int n -> n
| _ -> raise TypeMismatch
 
and evalBool e r = match semtry e r with
  Bool b -> b
| _ -> raise TypeMismatch


and evalChar e r = match semtry e r with
  Char c -> c
| _ -> raise TypeMismatch



(*vedere pagina 4 sul controllo delle variabili, serve a uscire dal loop?*)
(* espressione che produce, vecchio ambiente, nuovo ambiente*)

and controllerFV (e,d1,d2) = match e with
    Eint e1 -> d2
  | Echar e1 -> d2
  | Val v -> bind (d2, v, applyenv (d1,v)) 
  | True | False | Empty -> d2
  | Sum(e1,e2) |Diff(e1,e2) |Times(e1,e2) |And(e1,e2) |Or(e1,e2) 
  | Eq(e1,e2)  |Less(e1,e2) |Cons(e1,e2) |Epair(e1,e2) |Appl(e1,e2) 
      ->  controllerFV (e1,d1,(controllerFV (e2,d1,d2)))
  | Head e1 | Tail e1 | Fst e1 | Snd e1 | Not e1 -> controllerFV (e1,d1,d2) 
  | Ifthenelse (b,e1,e2) -> controllerFV (b,d1,(controllerFV (e1,d1,(controllerFV(e2,d1,d2)))))
  | Let (x, e1, e2) -> controllerFV (e1,d1,( controllerFV (e2,d1,(bind (d2, x, Undefined)))))
  | Rec (y, (Fun(x,t) as t1)) -> controllerFV (t1,d1,d2)
  | Fun (x, e1) -> controllerFV (e1,d1,(bind (d2,x,Undefined)))
  | Try (e1,i,e2)-> controllerFV (e1,d1,( controllerFV (e2,d1,(bind (d2, i, Undefined)))))
  | Raise i -> d2

  | _->failwith "controllo sul rec, manca il match completo"


(*ricorsione vedere pagina 4 prima delle regole della semantica *)
(* espressione che produce, vecchio ambiente, nuovo ambiente*)

and sub (e,oldV,newV)  = match e with
     Eint t -> e
  | Echar t -> e
  | Val i -> if i = oldV then newV else Val i   
  | True | False | Empty -> e 
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


and semtry e r =let rec sem e r pila =  match e with
   Eint n -> Int n
  | Echar c -> Char c  
  | Val x ->  applyenv (r,x)
  | Sum   (e1,e2) -> Int (evalInt e1 r + evalInt e2 r) 
  | Diff  (e1,e2) -> Int (evalInt e1 r - evalInt e2 r)
  | Times (e1,e2) -> Int (evalInt e1 r * evalInt e2 r)
  | True  -> Bool true
  | False -> Bool false
  | Eq (e1,e2) -> (match sem e1 r pila, sem e2 r pila with
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
  | Head e1 -> let a = sem e1 r pila in 
               let b = (match a with
                        List [] -> failwith "Lista vuota" 
                      | List (hd::tl) -> hd  
                      | _ -> raise TypeMismatch)         
               in b 
  |Tail e1 -> let a = sem e1 r pila in 
              let b = (match a with
                       List [] -> failwith "Lista vuota" 
                     | List (hd::tl) -> tl  
                     | _ -> raise TypeMismatch)         
              in List b 
   | Cons (e1, e2) ->(let a = sem e2 r pila in 
      let b = (match a with                     
      |(List l) ->(match l with
                   [] -> (sem e1 r pila)::[]  
                  |(hd::tl) -> (match sem e1 r pila , hd  with
                                 Int a,  Int  b -> (Int a)::l 
                                 | Bool a, Bool b -> (Bool a)::l
                                 | Char a, Char b -> (Char a)::l
                                 | Pair (Int a , Int b), Pair (Int c , Int d) -> (Pair (Int a,Int b))::l
                                 | Pair (Bool a , Bool b), Pair (Bool c , Bool d) -> (Pair (Bool a, Bool b))::l
                                 | Pair (Char a , Char b), Pair (Char c , Char d) -> (Pair (Char a, Char b))::l                                  
                                 | Pair (Int a , Char b), Pair (Int c , Char d) -> (Pair (Int a , Char b))::l
                                 | Pair (Bool a , Char b), Pair (Bool c , Char d) -> (Pair (Bool a , Char b))::l
                                 | Pair (Int a , Bool b), Pair (Int c , Bool d) -> (Pair(Int a , Bool b))::l
                                 | Pair (Char a , Bool b), Pair (Char c , Bool d) -> (Pair  (Char a , Bool b))::l 
                                 | Pair (Bool a , Int b), Pair (Bool c , Int d) -> (Pair (Bool a , Int b))::l
                                 | Pair (Char a , Int b), Pair (Char c , Int d) -> (Pair (Char a , Int b))::l                                  
                                 | _ as a,  List b -> (match a, List.hd b with
                                        List [Int a],  Int  b -> (List [Int a])::l 
                                      | List [Bool a], Bool b -> (List [Bool a])::l
                                      | List [Char a], Char b -> (List [Char a])::l                                      
                                      | List [Pair (Int a , Int b)], Pair (Int c , Int d) -> (List [Pair (Int a , Int b)])::l
                                      | List [Pair (Bool a , Bool b)], Pair (Bool c , Bool d) -> (List [Pair (Bool a , Bool b)])::l
                                      | List [Pair (Char a , Char b)], Pair (Char c , Char d) -> (List [Pair (Char a , Char b)])::l                                  
                                      | List [Pair (Int a , Char b)], Pair (Int c , Char d) -> ( List [Pair (Int a , Char b)])::l
                                      | List [Pair (Bool a , Char b)], Pair (Bool c , Char d) -> (List [Pair (Bool a , Char b)])::l
                                      | List [Pair (Int a , Bool b)], Pair (Int c , Bool d) -> ( List [Pair (Int a , Bool b)])::l
                                      | List [Pair (Char a , Bool b)], Pair (Char c , Bool d) -> (List [Pair (Char a , Bool b)])::l 
                                      | List [Pair (Bool a , Int b)], Pair (Bool c , Int d) -> (List [Pair (Bool a , Int b)])::l
                                      | List [Pair (Char a , Int b)], Pair (Char c , Int d) -> (List [Pair (Char a , Int b)])::l                                     
                                      |_-> failwith "errore liste di liste sem")
                                 | Undefined, Undefined -> (Undefined)::l
                                 |_ -> failwith "Errore liste"))
                      |_ -> failwith "errore liste 2")
                      in  List b )

  |Epair (e1,e2) -> Pair ( sem e1 r pila, sem e2 r pila) 
  |Fst e -> ( match (sem e r pila) with
           Pair (a, b) -> a
              |_-> raise TypeMismatch)
  |Snd e -> ( match (sem e r pila) with
           Pair (a, b) -> b
          |_-> raise TypeMismatch) 
  | Ifthenelse(e0,e1,e2) -> if evalBool e0 r then sem e1 r pila else sem e2 r pila
  | Let (x,e1,e2) -> sem e2 (bind (r,x,(sem e1 r pila))) pila
  | Rec(y,(Fun(x,e1))) -> 
          let newValue = (sub (e1,y,(Rec(y,Fun(x,e1))))) in
            Closure(Fun(x,newValue), controllerFV (newValue,r,emptyenv)) 
  | Fun (x,e1) -> Closure ((Fun(x,e1)), controllerFV (e1,r,emptyenv)) 
  | Appl (e1,e2) -> (match sem e1 r pila with
                Closure ((Fun(x,f)),d) -> (sem f (bind (d,x,(semtry e2 r))) pila)
               | _ -> failwith "coddati")  

  | Try (e1,i,e2) -> sem e1 r ((i,e2)::pila)
  
  | Raise id -> let (n, s) = controllerTry id pila in sem n r s
  |_-> failwith "problema guard per via del rec in sem"     

in sem e r ([]:(ide*exp)list)


and 
(*Contolla la lista delle eccezioni, non toccare è fragile !!!!! *)
controllerTry n s = match s with
    [] -> failwith "Eccezione non presente nello stack"
  | (a,b)::tl -> if a = n then (b,tl) else controllerTry n tl;;




semtry(
  Try(    
    (Try (
       (Ifthenelse( Eq(Val (Ide "x"), Echar 'c'),
        Raise (Ide "ecc1"),
        Raise (Ide "ecc2")
              )),
            Ide "ecc2",
            Eint 3)),
           Ide "ecc1",
           Eint 4)
)
(bind (emptyenv,(Ide "x"), Char 'c'));;
