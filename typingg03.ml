(*Progetto Gruppo con identificativo g03
Giorgia Campanile 65140
Marta Pibiri 65228
Stefano Raimondo Usai 65196
*)



(*Inferenza di tipo*)


(*Insieme tipo identificatore*)
type ide = Ide of string;;


(*Insieme tipi exp, espressioni di dati astratti eterogeni*)
type exp =
  Val of ide                     (*identificatore di variabile*)
| Eint of int                    (*tipo espressione intero*)
| Echar of char                  (*tipo espressione carattere*)
| True                           (*tipo Bool vero*)
| False                          (*tipo Bool falso*)
| Empty                          (*tipo espressione lista vuota*)
| Sum of exp * exp               (*parola identificatrice di somma, appartiene all'insieme degli operatori*)
| Diff of exp * exp              (*parola identificatrice di differenza, appartiene all'insieme degli operatori*)
| Times of exp * exp             (*parola identificatrice di moltiplicazione, appartiene all'insieme degli operatori*)
| And of exp * exp               (*parola identificatrice per operatore logico dell'AND *)
| Or of exp * exp                (*parola identificatrice per operatore logico dell'OR *)
| Not of exp                     (*parola identificatrice per operatore logico del NOT *)
| Eq of exp * exp                (*parola identificatrice per operatore logico dell' equivalenza*)
| Less of exp * exp              (*parola identificatrice per operatore di confronto del minore (<)*)
| Cons of exp * exp              (*parola identificatrice del costruttore della lista (::)*)
| Head of exp                    (*parola identificatrice della testa di una lista*)
| Tail of exp                    (*parola identificatrice della coda di una lista*)
| Fst of exp                     (*parola identificatrice del primo elemento di una coppia*)
| Snd of exp                     (*parola identificatrice del secondo elemento di una coppia*)
| Epair of exp * exp             (*parola identificatrice di una coppia*)
| Ifthenelse of exp * exp * exp  (*parola identificatrice del costrutto "if then else"*)
| Let of ide * exp * exp         (*parola identificatrice del delimitatore di blocco indicante l'inizio*)
| Fun of ide * exp               (*parola identificatrice di una funzione*)
| Appl of exp * exp              (*parola identificatrice utilizzata da una funzione per poter associare un valore ad un identificatore*)
| Rec of ide * exp               (*parola identificatrice che permette ad una funzione di essere ricorsiva*)
;;


(*Insieme dei tipi exp*)
type etype =
  TBool
| TInt
| TChar
| TVar of string
| TPair of etype * etype
| TList of etype list
| TFun of etype * etype;;



(*Nomi delle eccezioni*)
exception UndefinedIde of ide;;
exception TypeMismatch ;;


(*Le due funzioni servono a creare una referenza per ogni nuova variabile non tipata creata*)
let nextsym = ref (-1);;
let newvar = fun () -> nextsym := !nextsym + 1 ;
  TVar ("?T" ^ string_of_int (!nextsym));;


(*FUNZIONI PER L'AMBIENTE DEI TIPI*)
(*Crea l'ambiente dei tipi vuoto*)
let newtypenv = ([]:(ide*etype)list);;


(*La funzione applica il tipo all'ambiente, restituendo un ambiente con il tipo relativo*)
let rec applytypenv (e:(ide*etype)list) (Ide i) =  match e with
    [] -> failwith "empty environment 1"
   | ((Ide a),(b:etype))::[] -> if a = i then b else  failwith "empty environment 2"
  | ((Ide a),(b:etype))::tl -> if a = i then b else applytypenv tl (Ide i);;
  

(*La funzione associa un tipo e un identificatore all'ambiente dei tipi*)
let rec bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =match e with
    []-> (i,t)::[]
    |(i1,t1)::[] -> if i=i1 then (i1,t)::[] else (i,t)::(i1,t1)::[]      
    |(i1,t1)::tl -> if i=i1 then (i1,t)::tl else (i1,t1)::(bindtyp tl i t) ;; 


(*INFERENZA DI TIPO*)
(*Questa è la  costruzione di un insieme di vincoli per i tipi.
Il primo argomento è l'espressione da analizzare, il secondo argomento è un ambiente di tipi,
con tipo (ide * etype) list. Il risultato è un valore di tipo etype * (etype * etype) list,
in cui il primo elemento è il tipo di espressione e il secondo elemento è l'elenco di vincoli.
Secondo le specifiche di progetto si interpretano le regole sull'inferenza di tipo*)
let rec tconstraints e environment =  match e with
  | Val e1 -> (applytypenv environment e1, [])          
  | Eint e1 -> (TInt, [])
  | Echar e1 -> (TChar,[])
  | True | False -> (TBool, [])
  | Empty -> (TList [newvar()], [])        
  | Sum   (e1,e2)| Diff  (e1,e2) | Times (e1,e2) ->
      let (t1,c1) = tconstraints e1 environment in
      let (t2,c2) = tconstraints e2 environment in
      let c = [(t1,TInt); (t2,TInt)] in
        (TInt, c @ c1 @ c2)
  |And (e1,e2)|Or (e1,e2) -> 
      let (t1,c1) = tconstraints e1 environment in
      let (t2,c2) = tconstraints e2 environment in
      let c = [(t1,TBool);(t2,TBool)] in
        (TBool, c @ c1 @ c2)
  |Not e1 ->
     let (t1,c1) = tconstraints e1 environment in
       (TBool, [(t1,TBool)]@c1)
  | Eq (e1,e2)->      
      let (t1,c1) = tconstraints e1 environment in
      let (t2,c2) = tconstraints e2 environment in
      let c = [(t1,t2);(t1,t1);(t2,t2)]  in
        (TBool, c @ c1 @ c2)
  |Less (e1,e2) ->
     let (t1,c1) = tconstraints e1 environment in
     let (t2,c2) = tconstraints e2 environment in
     let c = [(t1,TInt); (t2,TInt)] in
       (TBool, c @ c1 @ c2)
  | Cons(e1,e2) ->
        let (t1,c1)= tconstraints e1 environment in
        let (t2,c2) = tconstraints e2 environment in 
        let c =  [(t1,t1);(t2, TList [t1])] in
          (TList [t1], (c@c1@c2)) 
  | Head l -> 
      let a = newvar() in
      let (t1,c1) = tconstraints l environment in
        (match t1 with
             (TList [l1]) -> (l1, ([(t1, TList [l1])]@c1))
           | TVar n -> (a, ([(TList [a], t1)]@c1))
             | _ -> failwith "tconstraints: error head inference")             
  | Tail l ->
      let a = newvar() in
      let (t1,c1) = tconstraints l environment
      in (match t1 with
              TList [l1] -> (t1, (c1))
            | TVar n ->  (TList[t1], ([TList[a],t1]@c1))
            | _ -> failwith "tconstraints: error tail inference")                       
  |Epair (e1,e2) -> 
     let (t1, c1) = tconstraints e1 environment in
     let (t2, c2) = tconstraints e2 environment in
     let c = [(t1, t1); (t2, t2 )] in
       (TPair(t1,t2), c@c1@c2)         
  | Fst e ->
      let a = newvar() in
      let (t1,c1) = tconstraints e environment
      in (match t1 with
              TPair(f,s) -> (f, ([t1, TPair(f,s)]@c1))
            | TVar n -> (a, [t1, TPair(a,newvar())]@c1)
            | _ -> failwith "tconstraints: error fst pair inference")           
  | Snd e ->
      let a = newvar() in
      let (t1, c1) = tconstraints e environment
      in (match t1 with
              TPair(f,s) -> (s, ([(t1, TPair(f,s))]@c1))
            | TVar n ->  (a, [t1, TPair(newvar(),a)]@c1)
            | _ -> failwith "tconstraints: error snd pair inference")               
  |Ifthenelse (e0,e1,e2) -> 
     let (t0,c0) = tconstraints e0 environment in
     let (t1,c1) = tconstraints e1 environment in
     let (t2,c2) = tconstraints e2 environment in
     let c = [(t0,TBool); (t1,t2)] in
         (t1, c @ c0 @ c1 @ c2)              
  | Let (x,e1,e2) -> let a = newvar() in
    let (t1,c1) = tconstraints e1 environment in
    let (t2,c2) = tconstraints e2 (bindtyp environment x a) in
      (t2, ([(t1, a)]@c1@c2))        
  | Fun(x,t) ->
      let a = newvar() in
      let (t1, c1) = tconstraints t (bindtyp environment x a)
      in (TFun(a,t1),c1)             
  | Appl(e1,e2) ->
      let a = newvar()
      in let (t1, c1) = tconstraints e1 environment
      in let (t2, c2) = tconstraints e2 environment
      in (a, ([t1, TFun(t2,a)]@c1@c2))           
  | Rec(y,f) -> let a = newvar() in
      (match f with
           Fun(x,t) -> let (t1, c1) =  tconstraints f (bindtyp environment y a)
           in (t1, ([a,a]@c1))
         | _ -> failwith "tconstraints: error Rec inference");;


(*SECONDA PARTE DELL'INFERENZA*)
(*Funzioni di appoggio per risolvere l'insieme dei vincoli: subst_app, subst e occurs*)
(*Applica la sostituzione dell'identificatore di un tipo con l'identificatore corretto*)
let rec subst_app n o e = match e with
    TInt | TChar | TBool -> e
  | TVar y -> if y=o then n else TVar y
  | TFun (t1,t2) -> TFun (subst_app n o t1 , subst_app n o t2)
  | TPair (t1,t2) ->TPair  (subst_app n o t1 , subst_app n o t2)
  | TList [l] -> TList [subst_app n o l]  
  |_-> failwith "subst_app: error ";;
  
  
(*Sostituisce un tipo per un identificatore di tipo in una lista di vincoli*)
(*l->Lista vincoli, n-> nuovo tipo, o-> vecchio tipo*)
let rec subst l n o = List.fold_right (fun (a,b) c -> (subst_app n o a, subst_app n o b)::c) l [];;  


(*Verifica il nome di un tipo *)
let rec occurs name typ = match typ with
    TInt | TBool |TChar -> false
  | TVar n1 -> n1=name
  | TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TList [l] ->   occurs name l
  |_-> failwith "occurs: error check";;



(*Funzione che unisce le coppie dei vincoli*)
let rec unify constrs = match constrs with
    [] -> []
  | (t1,t2)::tl  -> match t1, t2 with
        TInt,TInt | TBool,TBool | TChar,TChar -> unify tl
      | TVar id1, TVar id2 -> if id1 = id2 then unify tl 
          else (t1, t2)::unify (subst tl t2 id1)
      | TVar id, _ -> if not (occurs id t2) 
          then (t1, t2)::unify (subst tl t2 id )
          else failwith "unify: error occurs 1"
      | _, TVar id ->   if not (occurs id t1) 
          then (t2, t1)::unify (subst tl t1 id )
          else failwith "unify: error occurs 2"
      | TFun(a,b), TFun(c,d) | (TPair(a,b),TPair(c,d)) -> unify ((a,c)::(b,d)::tl)
      | TList [l1], TList [l2] -> unify ((l1,l2)::tl)
      | _ ->(t1,t2)::tl ;;

(*Funzione che restituisce l'inferenza dell'espressione*)
let rec typeinf e = 
let (e1,e2) = tconstraints e newtypenv in 
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
(*risolve i vincoli dell'espressione con l'ambiente *)
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "typeinf: resolve - I can not infer"
and 
(*typecheck per la risoluzione dei vincoli*)
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "typeinf: typecheck - error typesmatch";;


