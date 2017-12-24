type ide = Ide of string;;
 
type exp =
    N of int
  | Val of ide
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | True
  | False
  | Eq of exp * exp
  | Leq of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | If of exp * exp * exp
  | Let of ide * exp * exp
  | Letrec of ide * exp * exp
  | Fun of ide * exp
  | Apply of exp * exp
;;

type etype = 
    TBool 
  | TInt 
  | TVar of string 
  | TFun of etype * etype;;


let nextsym = ref (-1);;
let gentide = fun () -> nextsym := !nextsym + 1; TVar ("?T" ^ string_of_int (!nextsym));;


(* FUNZIONI PER L'AMBIENTE DEI TIPI*)
(*crea l'ambiente dei tipi vuoto*)
let emptyenv = ([]:(ide*etype)list);;

(*applica il tipo all'ambiente, restituendo un tipo...*)
(*assolutamente da rivedere*)
let rec applyenv e i =  match e,i with
    ((Ide a),(b:etype))::tl,(Ide i) -> if a = i then b else applyenv tl (Ide i)
  |_-> failwith "muori";;


(*associa un tipo e un identificatore all'ambiente dei tipi*)
let bind (e:(ide*etype)list) (i:ide) (t:etype) =(i,t)::[];; 


let rec tconstraints e tr = match e with
  N n -> (TInt,[])
| Val x ->  (applyenv tr x,[])
| Add (e1,e2)
| Sub (e1,e2)
| Mul (e1,e2)
| Div (e1,e2) ->
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TInt, c @ c1 @ c2)
| True
| False -> (TBool,[])
| Eq (e1,e2)
| Leq (e1,e2) ->
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 tr in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)
| Not e1 ->
    let (t1,c1) = tconstraints e1 tr in
    let c = [(t1,TBool)] in
    (TBool, c @ c1)
| And (e1,e2)
| Or (e1,e2) ->
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 tr in
    let c = [(t1,TBool); (t2,TBool)] in
    (TBool, c @ c1 @ c2)
| If(e0,e1,e2) ->
    let (t0,c0) = tconstraints e0 tr in
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 tr in
    let c = [(t0,TBool); (t1,t2)] in
    (t1, c @ c0 @ c1 @ c2)
| Let (x,e1,e2) ->
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 (bind tr x t1) in
    (t2, c1 @ c2)
| Letrec (x,e1,e2) ->
    let tx = gentide() in
    let (t1,c1) = tconstraints e1 (bind tr x tx) in
    let (t2,c2) = tconstraints e2 (bind tr x tx) in
    let c = [(tx,t1)] in
    (t2, c @ c1 @ c2)
| Fun (x,e1) ->
    let tx = gentide() in
    let (t1,c1) = tconstraints e1 (bind tr x tx) in
    (TFun (tx,t1), c1)
| Apply (e1,e2) ->
    let tx = gentide() in
    let (t1,c1) = tconstraints e1 tr in
    let (t2,c2) = tconstraints e2 tr in
    let c = [(t1,TFun(t2,tx))] in
    (tx, c @ c1 @ c2)
;;


let rec applysubst1 t0 x t = match t0 with
    TInt -> TInt
  | TBool -> TBool
  | TVar y -> if y=x then t else TVar y
  | TFun (t1,t2) -> TFun (applysubst1 t1 x t, applysubst1 t2 x t)
;;
 
let rec applysubst l x t = match l with
    [] -> []
  | (t1,t2)::l' -> (applysubst1 t1 x t, applysubst1 t2 x t)::(applysubst l' x t)
;;
 
let rec occurs x t = match t with
  TInt
| TBool -> false
| TVar y -> x=y
| TFun (t1,t2) -> (occurs x t1) || (occurs x t2)
;;
 
let rec unify  l = match l with
  [] -> []
| (TInt,TInt)::l' -> unify l'
| (TBool,TBool)::l' -> unify l'
| (TVar x, t)::l' ->
    if occurs x t then failwith "Occurs check"
    else (TVar x,t)::(unify (applysubst l' x t))
| (t, TVar x)::l' ->
    if occurs x t then failwith "Occurs check"
    else (TVar x,t)::(unify (applysubst l' x t))
| (TFun(t1,t2),TFun(t1',t2'))::l' ->
    unify ((t1,t1') :: (t2,t2') :: l')
| _ -> failwith "Unsolvable constraints"
;;


let type_inference e =
  let rec resolve t s = (match s with
    [] -> t
  | (TVar x, t')::s' -> resolve (applysubst1 t x t') s'
  | _ -> failwith ("Ill-formed substitution")) in
  let (t,c) = tconstraints e emptyenv in
  resolve t (unify c)
;;


let e0 = Let(Ide "succ",
	     Fun(Ide "x", Add(Val(Ide "x"), N 1)),
	     Apply(Val(Ide "succ"),N 8));;

let (t0,c0) = tconstraints e0 emptyenv;;

unify c0;;

type_inference e0;;


let e1 = N 1;;
let e2 =  True;;
let e3 = False;;
type_inference e3;;
