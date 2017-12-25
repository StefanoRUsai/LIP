(* Progetto gruppo con identificativo g03*)
(*#use evalg03.ml;;*)
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
| Pair of exp * exp              (*coppia *)
| Ifthenelse of exp * exp * exp  (*condizione?*)
| Let of ide * exp * exp         (*Inizio delimitatore Blocco?*)
| Fun of ide * exp               (*funzione*)
| Appl of exp * exp              (*applicazione*)
| Rec of ide * exp;;             (* funzione ricorsiva*)


type etype =
  TBool
| TInt
| TVar of string
| TPair of etype * etype
| TList of etype list
| TFun of etype * etype;;


(* USARE I PUNTATORI IN OCAML*)
(* puntatore per nuova variabile con reference*)
let nextsym = ref (-1);;
(* creazione nuova variabile tramite il sistema puntatore*)
let newvar = fun () -> nextsym := !nextsym + 1 ;
  TVar ("?T" ^ string_of_int (!nextsym));;

(* FUNZIONI PER L?AMBIENTE DEI TIPI*)
(*crea l'ambiente dei tipi vuoto*)
let newtypenv = ([]:(ide*etype)list);;

(*applica il tipo all'ambiente, restituendo un tipo...*)
(*assolutamente da rivedere*)
let rec applytypenv (e:(ide*etype)list) (Ide i) =  match e,i with
    [],_ -> failwith "ambiente vuoto" 
  |  ((Ide a),(b:etype))::tl,i -> if a = i then b else applytypenv tl (Ide i);;
  

(*associa un tipo e un identificatore all'ambiente dei tipi*)
let bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =(i,t)::e;; 


(*vincoli di tipaggio, ci si prova*)



let rec  tconst e tr = match e with
    Eint n ->(TInt,[])
  | Val  x -> (applytypenv tr x, [])
  | Sum   (e1,e2)
  | Diff  (e1,e2)
  | Times (e1,e2) ->
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TInt, c @ c1 @ c2)
  |True | False -> (TBool, [])
  |Empty -> (TList [],[])
  |And (e1,e2)|Or (e1,e2) -> 
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TBool); (t2,TBool)] in
    (TBool, c @ c1 @ c2)
  |Not e1 ->
    let (t1,c1) = tconst e1 tr in
    let c = [(t1,TBool)] in
    (TBool, c @ c1)
  | Eq (e1,e2)->      
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,t1); (t2,t2)] in
    (TBool, c @ c1 @ c2)  
  |Less (e1,e2) ->
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)
  |Cons (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, (TList []) )] in
     (TList [], c@c1@c2)
  |Fst e -> 
     let (t1, c1) = tconst e tr in
     let c = [(t1, t1); (t2, t2 )] in
   (TPair(t1,t2), c@c1)   
  |Pair (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, t2 )] in
   (TPair(t1,t2), c@c1@c2)
  |Ifthenelse (e0,e1,e2) ->
    let (t0,c0) = tconst e0 tr in
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t0,TBool); (t1,t2)] in
    (t1, c @ c0 @ c1 @ c2)
 | Let (x,e1,e2) ->
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 (bindtyp tr x t1)in
    (t2, c1 @ c2)
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
 | Rec (x,e1) ->
    let tx = newvar() in
    let (t1,c1) = tconst e1 (bindtyp tr x tx) in
    let c = [(tx,t1)] in
     (t1, c @ c1 )

|_-> failwith "errore";;



let rec subst_app t0 i t = match t0 with
    TInt -> TInt
  | TBool -> TBool
  | TVar y -> if y=i then t else TVar y
  | TFun (t1,t2) -> TFun (subst_app t1 i t, subst_app t2 i t)
  | TPair (t1,t2) ->TPair (subst_app t1 i t, subst_app t2 i t)
  |_-> failwith "sostituzione ";;

let rec subst l i t = match l with
    [] -> []
  | (t1,t2)::tl -> (subst_app t1 i t, subst_app t2 i t)::(subst tl i t)

let rec occurs name tipo = match tipo with
  TInt | TBool -> false
| TVar n1 -> n1=name
| TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
| TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
|_-> failwith " occorrenza lista"
;;

*(| TList n -> false*)
 
let rec unify  l = match l with
  [] -> []
| (TInt,TInt)::tl -> unify tl
| (TBool,TBool)::tl -> unify tl
| (TVar x, t)::tl ->
    if occurs x t then failwith "Controllo occorrenze"
    else (TVar x,t)::(unify (subst tl x t))
| (t, TVar x)::tl ->
    if occurs x t then failwith "Controllo occorrenze"
    else (TVar x,t)::(unify (subst tl x t))
| (TFun(t1,t2),TFun(t11,t22))::tl ->
    unify ((t1,t11) :: (t2,t22) :: tl)
| _ -> failwith "Non esiste il vincolo";; 



let rec type_inference e =
  let rec resolve t s = (match s with
    [] -> t
  | (TVar x, t1)::s1 -> resolve (subst_app t x t1) s1
  | _ -> failwith ("non riesco a fare infeerenza")) in
  let (t,c) = tconst e newtypenv in
  resolve t (unify c)
;;



let e0 = Eint 8;;
type_inference e0;;


let e0 = (Eq(Eint 8, True));;

let (t0,c0) = tconst e0 newtypenv;;

unify c0;;

type_inference e0;;

let e1 = Let(Ide "succ",
	     Fun(Ide "x", Sum(Val(Ide "x"), Eint 1)),
	     Appl(Val(Ide "succ"),Eint 8));;


let (t0,c0) = tconst e1 newtypenv;;

unify c0;;
type_inference e1;;

let a = Pair (Eint 8, True);;

type_inference a;;

let (t0, c0) = tconst a newtypenv;;

unify c0;;

let c = Fst (Pair (Eint 6, Eint 7));;
type_inference c;;
