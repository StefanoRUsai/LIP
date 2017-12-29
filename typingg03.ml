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
| Epair of exp * exp              (*coppia *)
| Ifthenelse of exp * exp * exp  (*condizione?*)
| Let of ide * exp * exp         (*Inizio delimitatore Blocco?*)
| Fun of ide * exp               (*funzione*)
| Appl of exp * exp              (*applicazione*)
| Rec of ide * exp;;             (* funzione ricorsiva*)


type etype =
  TBool
| TInt
| Tchar
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
  | Val x -> (applytypenv tr x, [])
  | Sum   (e1,e2)| Diff  (e1,e2) | Times (e1,e2) ->
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TInt, c @ c1 @ c2)
  |Echar c -> (Tchar,[])
  |True | False -> (TBool, [])
  |Empty -> (TList [newvar()] ,[])
  |And (e1,e2)|Or (e1,e2) -> 
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TBool); (t2,TBool)] in
    (TBool, c @ c1 @ c2)
  |Not e1 ->
    let (t1,c1) = tconst e1 tr in
    (TBool, c1)
  | Eq (e1,e2)->      
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = if t1 = t2 then [(t1,t1); (t2,t2)] else
    failwith "puoi equiparare solo lo stesso tipo" in
    (TBool, c @ c1 @ c2)  
  |Less (e1,e2) ->
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)
  |Cons (e1,e2) ->
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2,t2)] in
     (TList [t1],c@c1@c2)  
  |Head (Cons (e1,e2)) -> 
     let (t1, c1) = tconst e1 tr in
     let c = [(t1, (TList [t1]))] in
     (t1, c@c1)
  |Tail (Cons (e1,e2)) -> 
     let (t1, c1) = tconst e2 tr in
     let c = [(TList [t1], t1)] in
     (t1, c@c1)
  |Epair (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, t2 )] in
   (TPair(t1,t2), c@c1@c2)
  |Fst (Epair (e1,e2)) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in    
     let c = [(t1, (TPair (t1,t2)))] in
     (t1, c@c1)
  |Snd (Epair (e1,e2)) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in    
     let c = [(t2, (TPair (t1,t2)))] in
   (t2, c@c2)
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
 | Rec (x, Fun(y,z)) -> (* da sistemare*)
    let tx = newvar() in
    let (t1,c1) = tconst z (bindtyp tr x tx) in
    (TFun (tx,t1), c1)

|_-> failwith "errore";;

let rec subst_app t0 i t = match t0 with
    TInt -> TInt
  | Tchar -> Tchar
  | TBool -> TBool
  | TVar y -> if y=i then t else TVar y
  | TFun (t1,t2) -> TFun (subst_app t1 i t, subst_app t2 i t)
  | TPair (t1,t2) ->TPair (subst_app t1 i t, subst_app t2 i t)
  | TList l -> if l = [TVar i] then TList [t] else TList l

;;

let rec subst l i t = match l with
    [] -> []
  | (t1,t2)::tl -> (subst_app t1 i t, subst_app t2 i t)::(subst tl i t)

let rec occurs name typ = match typ with
  TInt | TBool |Tchar -> false
| TVar n1 -> n1=name
| TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
| TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
| TList [TVar l] -> name=l
|_-> failwith " verifica"
;;

 
let rec unify  l = match l with
  [] -> []
  |(TInt,TInt)::tl -> unify tl
  |(Tchar,Tchar)::tl -> unify tl
  |(TInt, TList a)::tl -> unify tl                   
  |(TBool,TBool)::tl -> unify tl
  |(TBool, TList a)::tl -> unify tl                   
  |(Tchar, TList a)::tl -> unify tl                   
  |(TVar x, t)::tl ->
    if occurs x t then failwith "Controllo verifica"
    else (TVar x,t)::(unify (subst tl x t))
  |(t, TVar x)::tl ->
    if occurs x t then failwith "Controllo verifica"
    else (TVar x,t)::(unify (subst tl x t))
  |(TFun(t1,t2),TFun(t11,t22))::tl -> unify ((t1,t11) :: (t2,t22) :: tl)
  |(TPair(t1,t2),TPair(t11,t22))::tl -> unify ((t1,t11) :: (t2,t22) :: tl)
  |(t1,TPair(t11,t22))::tl -> if t1 = t11 then unify ((t1,t11) :: tl) 
    else unify ((t1,t22) :: tl)
  | (TList a, TList b)::tl -> unify tl
  | _ -> failwith "Non esiste il vincolo";; 



let rec typeinf e =
  let rec resolve t s = (match s with
    [] -> t
  | (TVar x, t1)::s1 -> resolve (subst_app t x t1) s1
  | _ -> failwith ("non riesco a fare infeerenza")) in
  let (t,c) = tconst e newtypenv in
  resolve t (unify c)
;;

let s = Let(Ide "prova",
	     Fun(Ide "x", Sum(Val(Ide "x"), Fst (Epair (Eint 8, Eint 5)))),
	     Appl(Val(Ide "prova"),Eint 8));;

let (t0, c0) = tconst s newtypenv;;

let l = Sum (Eint 5, True);;


typeinf l;;


let a = Fst (Pair ( Eint 2, Eint 5));;

let (t0, c0) = tconst a newtypenv;;

typeinf a;;

let b = Snd (Pair ( Eint 2, True));;

let (t0, c0) = tconst b newtypenv;;

typeinf b;;

let a = Eq (Echar 'c', True );;

let (t0, c0) = tconst a newtypenv;;
typeinf a;;

let a = Empty;;

tconst a newtypenv;;

[newvar()];;
TInt;;
TList [newvar()];;


let a = Not (True);;

typeinf a;;


let f = Empty;;
let (t0,c0) = tconst f newtypenv;;
typeinf f;;

let l = Sum( Eint 2 , Eint 3);;

let (t0,c0) = tconst l newtypenv;;
typeinf l;;


let lista = Cons (Eint 2, Empty);;
let (t0,c0) = tconst lista newtypenv;;

let lista = Cons (Eint 2, Empty);;
let lista2 = Cons (Eint 2, lista);;
let (t0,c0) = tconst lista2 newtypenv;;
typeinf lista;;
typeinf (Eint 4);;


let n = Eint 3;;

tconst n newtypenv;;


tconst (Eint 4) newtypenv;;

let m = Eq (True, Eint 2);;

typeinf m;;

let a = Cons (Eint 2,Empty);;

tconst a newtypenv;;


let n = Eint 3;;
typeinf n;;


let a = Empty;;
tconst a newtypenv;;

typeinf a;;

let a = Cons (Eint 2, Empty);;
typeinf a;;

let a = Head (Cons (Eint 2, Empty));;
let b = Tail (Cons (Eint 2, Cons (Eint 2, Empty)));;

tconst a newtypenv;;
tconst b newtypenv;;

typeinf b;;


let s = Let(Ide "prova",
	    Fun(Ide "x",
                Sum(Val(Ide "x"), Fst (Pair (Eint 8, Eint 5)))),
	     Appl(Val(Ide "prova"),Eint 8));;


let s = Rec(Ide "y",
	     Fun(Ide "x",
                Sum(Val(Ide "x"), Fst (Pair (Eint 8, Eint 5)))));;

tconst s newtypenv;;

let w = 
	     Fun(Ide "x",
                Sum(Val(Ide "x"), Eint 5));;

tconst w newtypenv;;
typeinf w                      ;;  (*  *)



let w =  Fun (Ide "x", Appl(Val(Ide "x"), Eint 5));;
typeinf w;;


let a= ( Cons (Eint 4, Empty));;

typeinf a;;
tconst a newtypenv;;

[4];;

tconst (Eint 5) newtypenv;;
typeinf (Eint 5);;

(typeinf (Eint 5)) = TInt;;

typeinf (Cons (Echar 'c',(Cons (Eint 2, (Cons (Echar 'c', Empty)))))) ;;
let (a,b) = tconst (Cons (Echar 'c',(Cons (Eint 2, (Cons (Echar 'c', Empty))))))  newtypenv;;
a;;
b;;
unify b;;


tconst (Eint 5) newtypenv;;
