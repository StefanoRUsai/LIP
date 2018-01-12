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

(*applica il tipo all'ambiente, restituendo un tipo...*)
(*assolutamente da rivedere*)
let rec applytypenv (e:(ide*etype)list) (Ide i) =  match e with
    [] -> failwith "ambiente vuoto"
 |  ((Ide a),(b:etype))::[] -> if a = i then b else failwith "ambiente vuoto"
  |  ((Ide a),(b:etype))::tl -> if a = i then b else applytypenv tl (Ide i);;
  

(*associa un tipo e un identificatore all'ambiente dei tipi*)

let rec bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =match e with
    []-> (i,t)::[]
  |(i1,t1)::[]  -> if i=i1 then  (i1,t)::[] else (i,t)::(i1,t1)::[]
  |(i1,t1)::tl -> if i=i1 then (i1,t)::tl else (i1,t1)::(bindtyp tl i t) ;; 


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
    let c = [(t1,TBool);(t2,TBool)] in
    (TBool, c @ c1 @ c2)
  |Not e1 ->
    let (t1,c1) = tconst e1 tr in
    (TBool, c1)
  | Eq (e1,e2)->      
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t1,t1);(t2,t2)]  in
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
 |Head (Cons (e1,e2)) -> 
     let (t1, c1) = tconst e1 tr in
     let c = [(t1, (TList [t1]))] in
     (t1, c@c1)
  |Tail e1 -> 
     let (t1, c1) = tconst e1 tr in
       (t1, c1)
  |Epair (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, t2 )] in
   (TPair(t1,t2), c@c1@c2)
  |Fst(Epair(e1,e2)) ->
      let c = (Epair(e1,e2)) in 
      let (t1,c1) = tconst c tr
        in (match t1 with
            (TPair(a,b)) -> (a, ([(TPair(a,b), TPair(a,b))]@
                     (c1)))
          | _ -> failwith " errore sulla coppia")
  |Snd (Epair (e1,e2)) ->
      let c = (Epair(e1,e2)) in 
      let (t1,c1) = tconst c tr
        in (match t1 with
            (TPair(a,b)) -> (b, ([(TPair(a,b), TPair(a,b))]@
                     (c1)))
          | _ -> failwith " errore sulla coppia")
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
 | Rec (x, Fun(i,e)) -> (* da sistemare*)
    let tx = newvar() in
    let (t1,c1) = tconst (Fun(i,e)) (bindtyp tr x tx) in
   (match t1 with
        TFun(a,b) -> (TFun(a,b), ([(TFun(a,b),tx)]@c1))
      |_->failwith "varie bestemmie quando non funziona")      

|_-> failwith "errore";;


let rec subst_app t0 i t = match t0 with
    TInt -> TInt
  | Tchar -> Tchar
  | TBool -> TBool
  | TVar y -> if y=i then t else TVar y
  | TFun (t1,t2) -> TFun (subst_app t1 i t, subst_app t2 i t)
  | TPair (t1,t2) ->TPair (subst_app t1 i t, subst_app t2 i t)
  | TList [l] ->( match l with
        TVar i -> TList [t]
      |_->subst_app t i l)
  |_-> failwith "errore sostituzione"
;;

let rec subst l i t = match l with
    [] -> []
  | (t1,t2)::tl -> (subst_app t1 i t, subst_app t2 i t)::(subst tl i t)

let rec occurs name typ = match typ with
  TInt | TBool |Tchar -> false
| TVar n1 -> n1=name
| TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
| TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
| TList [l] -> (match l with
      TVar l -> name = l
    |_->  occurs name l)
|_-> failwith " verifica/occurs "
;;

 
let rec unify  l = match l with
  [] -> []
  |hd::tl  ->( match hd with
                  t1,t2 -> (match t1,t2 with
                    (TInt,TInt) -> unify tl
                  |(Tchar,Tchar) -> unify tl               
                  |(TBool,TBool) -> unify tl
                  |(TVar a, TVar b) -> if a = b then unify tl else unify tl@[hd]
                  |(TVar n, _)  ->
                     if occurs n t2 then failwith "Controllo verifica"
                     else (t1,t2)::(unify (subst tl n t2))
                  |(_, TVar n)  ->
                     if  occurs n t1  then failwith "Controllo verifica"
                     else (t1,t2)::(unify (subst tl n t1))
                  |(TFun(t3,t4),TFun(t33,t44)) -> unify ((t3,t33) :: (t4,t44) :: tl)
                  |(TPair(t3,t4),TPair(t33,t44)) -> unify ((t3,t33) :: (t4,t44) :: tl)
                  | (TList [t3], TList [t4]) -> unify ((t3,t4)::tl)
                  | _ ->  hd::tl));; 

let rec typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | Tchar -> Tchar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "Errore type check";;



let rec typeinf e =
  let rec resolve t s = (match s with
    [] -> t
  |(TVar x, t1)::s1  | (t1, TVar x )::s1 -> resolve (typeCheck t x t1) s1      
  | _ -> failwith ("non riesco a fare infeerenza")) in
  let (t,c) = tconst e newtypenv in
  resolve t (unify c)
;;


typeinf (Sum (Eint 2, Eint 3));;


tconst (Sum (Eint 2, Eint 3)) newtypenv;;


typeinf (Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Appl(Rec(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 2))), Eint 2)));;




typeinf ( Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))));


