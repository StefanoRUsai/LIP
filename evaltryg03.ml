
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
    [] -> failwith "empty environment 1"
   | ((Ide a),(b:etype))::[] -> if a = i then b else  failwith "empty environment 2"
  | ((Ide a),(b:etype))::tl -> if a = i then b else applytypenv tl (Ide i);;
  
(*associa un tipo e un identificatore all'ambiente dei tipi, vedere specifiche di progeto*)
let rec bindtyp (e:(ide*etype)list) (i:ide) (t:etype) =match e with
    []-> (i,t)::[]
    |(i1,t1)::[] -> if i=i1 then (i1,t)::[] else (i,t)::(i1,t1)::[]      
    |(i1,t1)::tl -> if i=i1 then (i1,t)::tl else (i1,t1)::(bindtyp tl i t) ;; 


(*vincoli di tipaggio, ci si prova*)
(*Questa è la  costruzione di un insieme di vincoli per i tipi.
Il primo argomento è l'espressione da analizzare,  secondo argomento è un ambiente di tipi,
con tipo (ide * etype) list. Il risultato è un valore di tipo etype * (etype * etype) list,
in cui il primo elemento è il tipo di espressione e il secondo elemento è l'elenco di vincoli.
Secondo le specifiche di progetto si interpretano le regole sull'inferenza di tipo*)

(*INFERENZA DI TIPO*)
let rec tconstraints e enveremont =  match e with
  | Val e1 -> (applytypenv enveremont e1, [])          
  | Eint e1 -> (TInt, [])
  | Echar e1 -> (TChar,[])
  | True | False -> (TBool, [])
  | Empty -> (TList [newvar()], [])        
  | Sum   (e1,e2)| Diff  (e1,e2) | Times (e1,e2) ->
      let (t1,c1) = tconstraints e1 enveremont in
      let (t2,c2) = tconstraints e2 enveremont in
      let c = [(t1,TInt); (t2,TInt)] in
        (TInt, c @ c1 @ c2)
  |And (e1,e2)|Or (e1,e2) -> 
      let (t1,c1) = tconstraints e1 enveremont in
      let (t2,c2) = tconstraints e2 enveremont in
      let c = [(t1,TBool);(t2,TBool)] in
        (TBool, c @ c1 @ c2)
  |Not e1 ->
     let (t1,c1) = tconstraints e1 enveremont in
       (TBool, [(t1,TBool)]@c1)
  | Eq (e1,e2)->      
      let (t1,c1) = tconstraints e1 enveremont in
      let (t2,c2) = tconstraints e2 enveremont in
      let c = [(t1,t2);(t1,t1);(t2,t2)]  in
        (TBool, c @ c1 @ c2)
  |Less (e1,e2) ->
     let (t1,c1) = tconstraints e1 enveremont in
     let (t2,c2) = tconstraints e2 enveremont in
     let c = [(t1,TInt); (t2,TInt)] in
       (TBool, c @ c1 @ c2)
  | Cons(e1,e2) ->
        let (t1,c1)= tconstraints e1 enveremont in
        let (t2,c2) = tconstraints e2 enveremont in 
        let c =  [(t1,t1);(t2, TList [t1])] in
          (TList [t1], (c@c1@c2)) 
  | Head l -> 
      let a = newvar() in
      let (t1,c1) = tconstraints l enveremont in
        (match t1 with
             (TList [l1]) -> (l1, ([(t1, TList [l1])]@c1))
           | TVar n -> (a, ([(TList [a], t1)]@c1))
             | _ -> failwith "tconstraints: error head inference")             
  | Tail l ->
      let a = newvar() in
      let (t1,c1) = tconstraints l enveremont
      in (match t1 with
              TList [l1] -> (t1, (c1))
            | TVar n ->  (TList[t1], ([TList[a],t1]@c1))
            | _ -> failwith "tconstraints: error tail inference")                       
  |Epair (e1,e2) -> (*da rivedere se non funziona*)
     let (t1, c1) = tconstraints e1 enveremont in
     let (t2, c2) = tconstraints e2 enveremont in
     let c = [(t1, t1); (t2, t2 )] in
       (TPair(t1,t2), c@c1@c2)         
  | Fst e ->
      let a = newvar() in
      let (t1,c1) = tconstraints e enveremont
      in (match t1 with
              TPair(f,s) -> (f, ([t1, TPair(f,s)]@c1))
            | TVar n -> (a, [t1, TPair(a,newvar())]@c1)
            | _ -> failwith "tconstraints: error fst pair inference")           
  | Snd e ->
      let a = newvar() in
      let (t1, c1) = tconstraints e enveremont
      in (match t1 with
              TPair(f,s) -> (s, ([(t1, TPair(f,s))]@c1))
            | TVar n ->  (a, [t1, TPair(newvar(),a)]@c1)
            | _ -> failwith "tconstraints: error snd pair inference")               
  |Ifthenelse (e0,e1,e2) -> (*problema?*)
     let (t0,c0) = tconstraints e0 enveremont in
     let (t1,c1) = tconstraints e1 enveremont in
     let (t2,c2) = tconstraints e2 enveremont in
     let c = [(t0,TBool); (t1,t2)] in
         (t1, c @ c0 @ c1 @ c2)              
  | Let (x,e1,e2) -> let a = newvar() in
    let (t1,c1) = tconstraints e1 enveremont in
    let (t2,c2) = tconstraints e2 (bindtyp enveremont x a) in
      (t2, ([(t1, a)]@c1@c2))        
  | Fun(x,t) ->
      let a = newvar() in
      let (t1, c1) = tconstraints t (bindtyp enveremont x a)
      in (TFun(a,t1),c1)             
  | Appl(e1,e2) ->
      let a = newvar()
      in let (t1, c1) = tconstraints e1 enveremont
      in let (t2, c2) = tconstraints e2 enveremont
      in (a, ([t1, TFun(t2,a)]@c1@c2))           
  | Rec(y,f) -> let a = newvar() in
      (match f with
           Fun(x,t) -> let (t1, c1) =  tconstraints f (bindtyp enveremont y a)
           in (t1, ([a,a]@c1))
         | _ -> failwith "tconstraints: error Rec inference");;





(* Funzioni di appoggio per unificare i tipi
subst_app
subst
occurs*)


(*  sostituisce un tipo per un identificatore di tipo in un tipo. *)
(* vecchio tipo, nuovo tipo, etype. Questa funzione evita il loop recursion
nella sostituzione dei vincoli. Specialmente nelle liste. Diverso dal typecheck
usato per il resolve*)
let rec subst_app n o e = match e with
    TInt | TChar | TBool -> e
  | TVar y -> if y=o then n else TVar y
  | TFun (t1,t2) -> TFun (subst_app n o t1 , subst_app n o t2)
  | TPair (t1,t2) ->TPair  (subst_app n o t1 , subst_app n o t2)
  | TList [l] -> TList [subst_app n o l]  
  |_-> failwith "subst_app: error ";;
  
  
(*sostituisce un tipo per un identificatore di tipo in una lista di vincoli*)
(*lista vincoli, nuovo tipo, vecchio tipo*)
let rec subst l n o = List.fold_right (fun (a,b) c -> (subst_app n o a, subst_app n o b)::c) l [];;  

(*   dice se il nome di un tipo è verificato *)

let rec occurs name typ = match typ with
    TInt | TBool |TChar -> false
  | TVar n1 -> n1=name
  | TPair (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TFun (t1,t2) -> (occurs name t1) || (occurs name t2)
  | TList [l] ->   occurs name l
  |_-> failwith "occurs: error check";;

(*Seconda fase del progetto unisci i vincoli creati da tconst.
Si da una lista di vincoli
 Il risultato, di tipo (etype * etype) list.*)

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
          else failwith "unify: error occurs 1"
      | TFun(a,b), TFun(c,d) | (TPair(a,b),TPair(c,d)) -> unify ((a,c)::(b,d)::tl)
      | TList [l1], TList [l2] -> unify ((l1,l2)::tl)
      | _ ->(t1,t2)::tl ;;

(* funzione richiesta dal professore, non smontare per nessun motivo (forse sì... vediamo) *)

let rec typeinf e = 
let (e1,e2) = tconstraints e newtypenv in 
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
(* risolve i vincoli dell'espressione con l'ambiente *)
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "typeinf: resolve - I can not infer"
and 
(*typecheck per la risoluzione dei vincoli, diversa dalla funzione per
 le sostituzioni non modificare assolutamente. Più simile a quello nella pagina del prof
*)
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "typeinf: typecheck - error typesmatch";;

let rec typeinf_App e env= 
let (e1,e2) = tconstraints e env in 
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
(* risolve i vincoli dell'espressione con l'ambiente *)
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "typeinf_App: resolve - I can not infer"
and 
(*typecheck per la risoluzione dei vincoli, diversa dalla funzione per
 le sostituzioni non modificare assolutamente. Più simile a quello nella pagina del prof
*)
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "typeinf_App: typecheck - error typesmatch";;





 (*eccezioni per il try*)
exception UndefinedIde of ide;;
exception TypeMismatch ;;

(* funzioni di ambiente per l'interprete*)
let rec emptyenv =  (fun (x:ide) -> Undefined)
and applyenv ((r:env),(x:ide)) = r x
and bind (r, x, d) =  (fun y -> if y=x then d else applyenv (r,y));;



(* serve per controllare il tipo delle coppie 
nell'eq dell'interprete, da rivedere e controllare !!!!!!!!!!*)

let rec typeCheckEq (a,b) = match a,b with
      Undefined, Undefined -> true
  | Int _,Int _ -> true 
  | Bool _, Bool _ -> true 
  | Char _, Char _ -> true
  | List _, List _ -> true
  | Pair ((_ as a),(_ as b)), Pair ((_ as c),( _ as d) ) ->  typeCheckEq(a,c)&&typeCheckEq(b,d)
  |_-> failwith "non sono uguali, inutile che ci tenti";;



let newstack = ([]:(ide*exp)list);;

let rec expr e envE =
  match e with
      Undefined -> failwith "l'eval è di tipo undefined"
    | Int x -> Eint x
    | Bool b -> if b then True else False
    | Char c -> Echar c
    | List [] -> Empty
    | List (hd::tl) -> Cons(expr hd envE, expr (List tl) envE)
    | Pair(sx,dx) -> Epair(expr sx envE, expr dx envE)
    | _ -> failwith "errore nel ritorno dell'espressione";;





(* le funzioni che iniziano per eval sono un typecheck 
per l'interprete, per restituire un 
dato primivito della macchina ospite*)

let rec evalInt e r envType pila= match (sem_App e r envType pila ) with
  Int n -> n
| _ -> raise TypeMismatch
 
and evalBool e r envType pila= match sem_App e r envType pila with
  Bool b -> b
| _ -> raise TypeMismatch

and evalChar e r envType pila= match sem_App e r envType pila with
  Char c -> c
| _ -> raise TypeMismatch


(*vedere pagina 4 sul controllo delle variabili libere*)
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

(*Interprete, con  try raise. Il try e il raise funzionano tramite una pila di eccezioni
come in java, ogni volta che l'interprete legge l'espressione try
inserisce l'identificatore nel try e  la seconda espressione nella lista. *)

and  sem_App e r envType pila = match e with
    Eint n -> Int n
  | Echar c -> Char c  
  | Val x ->  applyenv (r,x)
  | Sum   (e1,e2) -> Int (evalInt e1 r envType pila + evalInt e2 r envType pila) 
  | Diff  (e1,e2) -> Int (evalInt e1 r envType pila - evalInt e2 r envType pila)
  | Times (e1,e2) -> Int (evalInt e1 r envType pila * evalInt e2 r envType pila)
  | True  -> Bool true
  | False -> Bool false
  | Eq (e1,e2) -> (match sem_App e1 r envType pila, sem_App e2 r envType pila with
                       Int a, Int b   -> Bool (a=b) 
                     | Bool a, Bool b -> Bool (a=b) 
                     | Char a, Char b -> Bool (a=b) 
                     | List [a], List [] -> Bool false
                     | List [], List [b] -> Bool false
                     | List a, List b -> 
                         if 
                           ((typeinf_App (expr (sem_App e1 r envType pila) r) envType)=
                           (typeinf_App (expr (sem_App e2 r envType pila) r) envType)) 
                         then Bool (a=b)
                       else failwith "le liste non sono dello stesso tipo"                     
                     | Pair(a,b), Pair (c,d) -> if  (typeCheckEq (a,c) &&  typeCheckEq (b,d))
                       then  Bool (a=c&&b=d) else failwith "le coppie non sono dello stesso tipo"
                     | Closure(a,b), Closure (c,d) -> Bool (a=c&&b==d)    
                     |Undefined, Undefined -> Bool (Undefined=Undefined)
                     |_-> raise TypeMismatch) 
   | Less (e1,e2) -> Bool (evalInt e1 r envType pila <= evalInt e2 r envType pila)
  | Not ne -> Bool (not (evalBool ne r envType pila)) 
  | And (e1,e2) -> Bool (evalBool e1 r envType pila && evalBool e2 r envType pila)
  | Or (e1,e2) -> Bool (evalBool e1 r envType  pila || evalBool e2 r envType pila)
  | Empty -> List []
  | Head e1 -> let a = sem_App e1 r envType pila in 
               let b = (match a with
                        List [] -> failwith "Lista vuota" 
                      | List (hd::tl) -> hd  
                      | _ -> raise TypeMismatch)         
               in b 
  |Tail e1 -> let a = sem_App e1 r envType pila in 
              let b = (match a with
                       List [] -> failwith "Lista vuota" 
                     | List (hd::tl) -> tl  
                     | _ -> raise TypeMismatch)         
              in List b 
    | Cons (e1, e2) -> 
        (if (not (typeinf_App (Cons ((expr (sem_App e1 r envType pila) r),
                                     (expr (sem_App e2 r envType pila) r)))  envType = TInt)) then
        (let a = sem_App e2 r envType pila in 
                                let b = (match a with                     
                                           |(List l) ->(match l with
                                                            [] -> (sem_App e1 r envType pila)::[]
                                                          |(hd::tl) as lista -> (sem_App e1 r envType pila)::lista)  
                                           |_->failwith "errore liste match typeinf")
                                in  List b ) else failwith "liste")
  |Epair (e1,e2) -> Pair ( sem_App e1 r envType pila, sem_App e2 r envType pila) 
  |Fst e -> ( match (sem_App e r envType pila) with
           Pair (a, b) -> a
              |_-> raise TypeMismatch)
  |Snd e -> ( match (sem_App e r envType pila) with
           Pair (a, b) -> b
          |_-> raise TypeMismatch) 
  | Ifthenelse(e0,e1,e2) -> if evalBool e0 r envType pila then sem_App e1 r envType pila else sem_App e2 r envType  pila
  | Let (x,e1,e2) -> sem_App e2 (bind (r,x,(sem_App e1 r envType pila)))
      (bindtyp (envType) (x) (typeinf_App e1 envType) ) pila

  | Rec(y,(Fun(x,e1))) -> 
          let newValue = (sub (e1,y,(Rec(y,Fun(x,e1))))) in
            Closure(Fun(x,newValue), controllerFV (newValue,r,emptyenv)) 
  | Fun (x,e1) -> Closure ((Fun(x,e1)), controllerFV (e1,r,emptyenv)) 
  | Appl (e1,e2) -> (match sem_App e1 r envType pila with
                         Closure ((Fun(x,f)),d) -> (sem_App f (bind (d,x,(sem_App e2 r envType pila)))  
                                                      (bindtyp envType x (typeinf_App (expr (sem_App e2 r envType pila) r) envType) ) pila))
  | Try (e1,i,e2) -> sem_App e1 r envType ((i,e2)::pila)  
  | Raise id -> let (n, s) = controllerTry id pila in sem_App n r envType s
  |_-> failwith "problema guard per via del rec in sem"     
(*Contolla la lista delle eccezioni, se l'eccezione è presente nella lista
viene gestita e restituisce la seconda espressione che si trova nel Try collegato
 al Raise...  non toccare è fragile !!!!! *)

and controllerTry n s = match s with
    [] -> failwith "Eccezione non presente nello stack"
  | (a,b)::tl -> if a = n then (b,tl) else controllerTry n tl;;


let rec semtry e r= sem_App e r newtypenv newstack;;



