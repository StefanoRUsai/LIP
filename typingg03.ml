
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
env = ide -> eval;;


 
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
    [] -> failwith "typeEnv: ambiente vuoto2"
   | ((Ide a),(b:etype))::[] -> if a = i then b else  failwith "typeEnv: ambiente vuoto1"    
  | ((Ide a),(b:etype))::tl -> if a = i then b else applytypenv tl (Ide i);;
  
(*associa un tipo e un identificatore all'ambiente dei tipi, vedere specifiche di progeto*)
let rec bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =match e with
    []-> (i,t)::[]
    |(i1,t1)::[] -> if i=i1 then (i1,t)::[] else (i,t)::(i1,t1)::[]      
    |(i1,t1)::tl -> if i=i1 then (i1,t)::tl else (i1,t1)::(bindtyp tl i t) ;; 


(*vincoli di tipaggio, ci si prova*)
(*Questa �� la  costruzione di un insieme di vincoli per i tipi.
Il primo argomento �� l'espressione da analizzare,  secondo argomento �� un ambiente di tipi,
con tipo (ide * etype) list. Il risultato �� un valore di tipo etype * (etype * etype) list,
in cui il primo elemento �� il tipo di espressione e il secondo elemento �� l'elenco di vincoli.
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
  |Cons (e1,e2) ->(
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c =  [ (t1,t1) ; (t2, TList [t1]) ] in
       (TList [t1],c@c1@c2)  )
  | Head l -> 
      let (l1, c1) =  tconst l  tr in
      let t1 = (match l1 with
          TList [t1]-> t1
                  |_-> failwith "errore head in inferenza")
      in 
      (t1, ([(TList [t1], TList [t1])]@c1)) 
  | Tail l ->
      let (l1,c1) =  tconst l tr in
      let  t1 = (match l1 with                        
                   TList _->  (l1, c1)
                   |_->failwith "errore tail in inferenza") in
        t1
  |Epair (e1,e2) -> 
     let (t1, c1) = tconst e1 tr in
     let (t2, c2) = tconst e2 tr in
     let c = [(t1, t1); (t2, t2 )] in
       (TPair(t1,t2), c@c1@c2)
| Fst e1 ->(
      let (t1,c1) = tconst e1 tr
        in (match t1 with
            (TPair(first,second)) -> 
              (first, ([(TPair(first,second), TPair(first,second))]@ c1))
              | TVar n ->(let a = newvar()  in (a, [TPair(a,newvar()), t1]@c1))
          | _ -> failwith "non �� una coppia"))
  | Snd e1 ->(
      let (t1,c1) = tconst e1 tr
        in (match t1 with
            (TPair(first,second)) -> 
              (second, ([(TPair(first,second), TPair(first,second))]@ c1))
              | TVar n ->(let a = newvar()  in (a, [TPair(a,newvar()),t1]@c1))
          | _ -> failwith "non �� una coppia"))     
  |Ifthenelse (e0,e1,e2) ->
     let (t0,c0) = tconst e0 tr in
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 tr in
    let c = [(t0,TBool); (t1,t2)] in
      (t1, c @ c0 @ c1 @ c2)
  | Let (x,e1,e2) ->
     let tx =newvar () in 
    let (t1,c1) = tconst e1 tr in
    let (t2,c2) = tconst e2 (bindtyp tr x t1) in
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
(* vecchio tipo, nuovo tipo, etype. Questa funzione evita il loop recursion
nella sostituzione dei vincoli. Specialmente nelle liste. Diverso dal typecheck
usato per il resolve*)
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


(*   dice se il nome di un tipo �� verificato *)

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

(* funzione richiesta dal professore, non smontare per nessun motivo (forse s��... vediamo) *)

let rec typeinf e = 
let (e1,e2) = tconst e newtypenv in 
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
(* risolve i vincoli dell'espressione con l'ambiente *)
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1  | (t1, TVar x )::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "non riesco a inferire"
and 
(*typecheck per la risoluzione dei vincoli, diversa dalla funzione per
 le sostituzioni non modificare assolutamente. Pi�� simile a quello nella pagina del prof
*)
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "Errore type check";;
