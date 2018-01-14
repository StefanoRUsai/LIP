(************************************************************)
(*                  ENVIRONMENT/AMBIENTE                    *)
(************************************************************)

(* "tipo valutazione dell'esprezzione" *)

(* Progetto gruppo con identificativo g03*)
(*Tipi exp, espressioni di dati astratti  eterogeni*)

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


type etype =
  TBool
| TInt
| TChar
| TVar of string
| TPair of etype * etype
| TList of etype list
| TFun of etype * etype;;



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


(* USARE I PUNTATORI IN OCAML*)
(* puntatore per nuova variabile con reference?*)

(* queste 2 funzioni creano ununa nuova variabile, secondo specifiche di progetto*)

let nextsym = ref (-1);;
let newvar = fun () -> nextsym := !nextsym + 1 ;
  TVar ("?T" ^ string_of_int (!nextsym));;

(* FUNZIONI PER L?AMBIENTE DEI TIPI*)
(*crea l'ambiente dei tipi vuoto, vedere specifiche del progetto*)
let newtypenv = ([]:(ide*etype)list);;


(*applica il tipo all'ambiente, restituendo un tipo, vedere specifiche di progetto*)
let rec applytypenv (e:(ide*etype)list) (Ide i) =  match e with
    [] -> failwith "ambiente vuoto"
  | ((Ide a),(b:etype))::tl -> if a = i then b else applytypenv tl (Ide i);;
  
(*associa un tipo e un identificatore all'ambiente dei tipi, vedere specifiche di progeto*)
let rec bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =match e with
    []-> (i,t)::[]
    |(i1,t1)::[] -> if i=i1 then (i1,t)::[] else (i,t)::(i1,t1)::[]      
    |(i1,t1)::tl -> if i=i1 then (i1,t)::tl else (i1,t1)::(bindtyp tl i t) ;; 


(*vincoli di tipaggio, ci si prova*)
(*Questa � la  costruzione di un insieme di vincoli per i tipi.
Il primo argomento � l'espressione da analizzare,  secondo argomento � un ambiente di tipi,
con tipo (ide * etype) list. Il risultato � un valore di tipo etype * (etype * etype) list,
in cui il primo elemento � il tipo di espressione e il secondo elemento � l'elenco di vincoli.
Secondo le specifiche di progetto si interpretano le regole sull'inferenza di tipo*)

(*INFERENZA DI TIPO*)

let rec  tconst e tr = match e with
    Eint n ->(TInt,[])
  | Val x -> (applytypenv tr x, [])
  |Echar c -> (TChar,[])
  |True | False -> (TBool, [])
  |Empty -> (TList [newvar()] ,[])
  | Sum   (e1,e2)| Diff  (e1,e2) | Times (e1,e2) ->
      let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
      (TInt, c @ c1 @ c2)
  |And (e1,e2)|Or (e1,e2) -> 
      let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TBool);(t2,TBool)] in
      (TBool, c @ c1 @ c2)
  |Not e1 ->
     let (t1,c1) = tconst e1 tr in
      (TBool, [(t1,TBool)]@c1)
  | Eq (e1,e2)->      
      let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,t2);(t1,t1);(t2,t2)]  in
      (TBool, c @ c1 @ c2)
  |Less (e1,e2) ->
     let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
      (TBool, c @ c1 @ c2)
  |Cons (e1,e2) ->
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c =  [ (t1,t1) ; (t2, TList [t1]) ] in
       (TList [t1],c@c1@c2)  
  | Head l -> 
      let (TList [t1], c1) =  tconst l  tr
      in (t1, ([(TList [t1], TList [t1])]@c1)) 
  | Tail l ->
      let (t1,c1) =  tconst l tr
      in (t1, c1)
  |Epair (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, t2 )] in
       (TPair(t1,t2), c@c1@c2)
  |Fst(Epair(e1,e2)) ->
     let c = (Epair(e1,e2)) in 
     let (t1,c1) = tconst c tr
      in (match t1 with
                (TPair(a,b)) -> (a, ([(TPair(a,b), TPair(a,b))]@(c1)))
              | _ -> failwith " errore sulla coppia")
  |Snd (Epair (e1,e2)) ->
     let c = (Epair(e1,e2)) in 
      let (t1,c1) = tconst c tr
      in (match t1 with
                (TPair(a,b)) -> (b, ([(TPair(a,b), TPair(a,b))]@(c1)))
              | _ -> failwith " errore sulla coppia")
  |Ifthenelse (e0,e1,e2) ->
     let (t0,c0) = tconst e0 tr in
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t0,TBool); (t1,t2)] in
      (t1, c @ c0 @ c1 @ c2)
  | Let (x,e1,e2) ->
     let tx =newvar () in 
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 (bindtyp tr x tx) in
    let c = [(t1,tx)] in   
      (t2, c @ c1 @ c2)
 | Fun (x,e1) ->
     let tx = newvar() in
    let (t1,c1) = tconst e1 (bindtyp tr x tx) in
      (TFun (tx,t1), c1)
 | Appl (e1,e2) ->
     let tx = newvar() in
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TFun(t2,tx))] in
      (tx, c @ c1 @ c2)    
 | Rec (x, Fun(i,e)) ->
     let tx = newvar() in
    let (t1,c1) = tconst (Fun(i,e)) (bindtyp tr x tx) in
   (match t1 with
        TFun (a,b) -> (TFun(a,b), ([(TFun(a,b),tx)]@c1))
      |_->failwith "varie bestemmie quando non funziona")      

 |_-> failwith "errore";;



(* Funzioni di appoggio per unificare i tipi
subst_app
subst
occurs*)


(*  sostituisce un tipo per un identificatore di tipo in un tipo. *)
(* vecchio tipo, nuovo tipo, etype*)
let rec subst_app n o e = match e with
    TInt -> e
  | TChar -> e
  | TBool -> e
  | TVar y -> if y=o then n else TVar y
  | TFun (t1,t2) -> TFun (subst_app n o t1 , subst_app n o t2)
  | TPair (t1,t2) ->TPair  (subst_app n o t1 , subst_app n o t2)
  | TList [l] -> TList [subst_app n o l]
  |_-> failwith "errore sostituzione";;
  
  
(*sostituisce un tipo per un identificatore di tipo in una lista di vincoli*)
(*lista vincoli, nuovo tipo, vecchio tipo*)
let rec subst l n o = List.fold_right (fun (a,b) c -> (subst_app n o a, subst_app n o b)::c) l [];;  


(*   dice se il nome di un tipo � verificato *)

let rec occurs name typ = match typ with
    TInt | TBool |TChar -> false
  | TVar n1 -> n1=name
  | TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TList [l] -> (match l with
                      TVar l -> name = l
                    |_->  occurs name l)
  |_-> failwith " verifica/occurs ";;

(*Seconda fase del progetto unisci i vincoli creati da tconst.
Si da una lista di vincoli
 Il risultato, di tipo (etype * etype) list.*)
let rec unify  l = match l with
    [] -> []
  |(t1,t2)::tl  -> (match t1,t2 with
                                 (TInt,TInt) -> unify tl
                               |(TChar,TChar) -> unify tl               
                               |(TBool,TBool) -> unify tl
                               | (TVar n1, TVar n2) -> if n1 = n2 then unify tl else unify tl@[(t1,t2)]
                               | (TVar id, _) -> if not (occurs id t2) 
                                 then (t1, t2)::(unify (subst tl t2 id ))
                                 else failwith "unify tvar 1"
                               | (_, TVar id) -> if not (occurs id t1) 
                                 then (t1, t2)::(unify (subst tl t1 id))
                                 else failwith "unify tvar 2"
                               |(TFun(t3,t4),TFun(t33,t44)) -> unify ((t3,t33) :: (t4,t44) :: tl)
                               |(TPair(t3,t4),TPair(t33,t44)) -> unify ((t3,t33) :: (t4,t44) :: tl)
                               | (TList [t3], TList [t4]) -> unify ((t3,t4)::tl)
                               | _ ->  (t1,t2)::tl);; 

(* funzione richiesta dal professore, non smontare per nessun motivo (forse s�... vediamo) *)

let rec typeinf e = 
let (e1,e2) = tconst e newtypenv in
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1  | (t1, TVar x )::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "non riesco a inferire"
and 
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "Errore type check";;



 
exception UndefinedIde of ide;;
exception TypeMismatch ;;

      

let rec emptyenv =  Env(fun x -> Undefined)
and bind (r, x, d) = Env (fun y -> if y=x then d else applyenv (r,y))
and applyenv ((Env r),x) = r x ;;



(* serve per controllare il tipo delle coppie 
nell'eq dell'Epair*)
let rec typeCheckEq (a,b) = match a,b with
      Undefined, Undefined -> true
  | Int _,Int _ -> true 
  | Bool _, Bool _ -> true 
  | Char _, Char _ -> true
  | List _, List _ -> true
  | Pair ((_ as a),(_ as b)), Pair ((_ as c),( _ as d) ) ->  typeCheckEq(a,c)&&typeCheckEq(b,d)
  |_-> failwith "non sono uguali, inutile che ci tenti";;


(*funzione di appoggio per controllare la testa di una lista vuota 
List.hd lancia eccezione non va bene*)

let rec evalInt e r = match (semtry e r) with
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
    Val v -> bind (d2, v, applyenv (d1,v)) 
  |Eint e1 -> d2
  |Echar e1 -> d2
  |True | False | Empty -> d2
  |Sum(e1,e2) |Diff(e1,e2) |Times(e1,e2) |And(e1,e2) |Or(e1,e2) 
  |Eq(e1,e2)  |Less(e1,e2) |Cons(e1,e2) |Epair(e1,e2) |Appl(e1,e2) 
      ->  controllerFV (e1,d1,(controllerFV (e2,d1,d2)))
  |Head e1 | Tail e1 | Fst e1 | Snd e1  | Not e1 -> controllerFV (e1,d1,d2) 
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

and  semtry e r =let rec sem e r pila =  match e with
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
                     | List a, List b -> if ((typeinf e1)=(typeinf e2)) then Bool (a=b)
                       else failwith "le liste non sono dello stesso tipo"
                     | Pair(a,b), Pair (c,d) -> if  (typeCheckEq (a,c) &&  typeCheckEq (b,d))
                       then  Bool (a=c&&b=d) else failwith "le coppie non sono dello stesso tipo"
                     | Closure(a,b), Closure (c,d) -> Bool (a=c&&b==d)    
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
    | Cons (e1, e2) -> (if (not (typeinf (Cons (e1, e2)) = TInt)) then 
                        (let a = sem e2 r pila in 
                                let b = (match a with                     
                                           |(List l) ->(match l with
                                                            [] -> (sem e1 r pila)::[]
                                                          |(hd::tl) as lista -> (sem e1 r pila)::lista))  
                                in  List b ) else failwith "liste")
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
(*Contolla la lista delle eccezioni, non toccare � fragile !!!!! *)
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
