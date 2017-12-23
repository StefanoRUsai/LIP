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
let rec applytypenv e (Ide i) =  match i with
    "Eint"  when e = newtypenv -> TInt 
  | "Ebool" when e = newtypenv -> TBool
  | "Pair"  when e = newtypenv -> TPair (applytypenv e (Ide i),applytypenv e (Ide i))
  | "List"  when e = newtypenv -> TList [applytypenv e (Ide i)] 
  | "Fun "  when e = newtypenv -> TFun (applytypenv e (Ide i), applytypenv e (Ide i))
  |_-> failwith "errore";;

(*associa un tipo e un identificatore all'ambiente dei tipi*)
let bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =(i,t)::[];; 

(*vincoli di tipaggio, ci si prova*)


let rec  tconst e t = match e with
    Eint n ->(TInt,[])
  | Val  x -> (applytypenv t x, [])
  | Sum   (e1,e2)
  | Diff  (e1,e2)
  | Times (e1,e2) ->
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 t in
    let c = [(t1,TInt); (t2,TInt)] in
    (TInt, c @ c1 @ c2)
  |True|False -> (TBool, [])
  |Empty -> (TList [],[])
  |And (e1,e2)
  |Or (e1,e2) ->
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 t in
    let c = [(t1,TBool); (t2,TBool)] in
    (TBool, c @ c1 @ c2)
  |Not e1 ->
    let (t1,c1) = tconst e1 t in
    let c = [(t1,TBool)] in
    (TBool, c @ c1)
  |Eq  (e1,e2)
  |Less (e1,e2) ->
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 t in
    let c = [(t1,TInt); (t2,TInt)] in
    (TBool, c @ c1 @ c2)   
  |Cons (e1,e2) -> 
     let (t1, c1) = tconst e1 t in
     let (t1, c2) = tconst e2 t in
     (TList (c1@c2), [])  
  |Ifthenelse (e0,e1,e2) ->
    let (t0,c0) = tconst e0 t in
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 t in
    let c = [(t0,TBool); (t1,t2)] in
    (t1, c @ c0 @ c1 @ c2)
 | Let (x,e1,e2) ->
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 (bindtyp t x t1)in
    (t2, c1 @ c2)
 | Fun (x,e1) ->
    let tx = newvar() in
    let (t1,c1) = tconst e1 (bindtyp t x tx) in
    (TFun (tx,t1), c1)
 | Appl (e1,e2) ->
    let tx = newvar() in
    let (t1,c1) = tconst e1 t in
    let (t2,c2) = tconst e2 t in
    let c = [(t1,TFun(t2,tx))] in
    (tx, c @ c1 @ c2)    
 | Rec (x,e1) ->
    let tx = newvar() in
    let (t1,c1) = tconst e1 (bindtyp t x tx) in
    let c = [(tx,t1)] in
     (t1, c @ c1 )

|_-> failwith "errore";;

