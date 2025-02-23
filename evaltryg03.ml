(*Progetto Gruppo con identificativo g03
Giorgia Campanile 65140
Marta Pibiri 65228
Stefano Raimondo Usai 65196
*)


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
| Try of exp * ide * exp         (*parola identificatrice che serve a dichiarare l'eccezione*)   
| Raise of ide                   (*parola identificatrice che serve a raccogliere l'eccezione*)
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

(*Insieme dei tipi eval*)
type eval =
  Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env
and env = ide -> eval;;



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

      
(*Le funzioni di ambiente per l'interprete sem, come da specifica del progetto*)
let rec emptyenv =  (fun (x:ide) -> Undefined)
and applyenv ((r:env),(x:ide)) = r x
and bind (r, x, e) =  ((fun y -> if y=x then e else applyenv (r,y)):env);;


(*Una funzione di appoggio per trasformare un eval in un exp*)
let rec expr e envE =
  match e with
      Undefined -> failwith "l'eval � di tipo undefined"
    | Int x -> Eint x
    | Bool b -> if b then True else False
    | Char c -> Echar c
    | List [] -> Empty
    | List (hd::tl) -> Cons(expr hd envE, expr (List tl) envE)
    | Pair(sx,dx) -> Epair(expr sx envE, expr dx envE)
    | _ -> failwith "errore nel ritorno dell'espressione";;


(*INFERENZA DI TIPO A SUPPORTO DI SEM*)
let rec tconstraintssem e environment environmentsem=  match e with 
    Val e1 -> (let (ide, const) = 
      try (true, (applytypenv environment e1, [])) 
      with _ -> (false, (TVar "", []))
    in if ide then const  else (match applyenv(environmentsem ,e1) with
                                    Closure(foo, env1) ->(match foo with
                                                              Fun(x,t) -> tconstraintssem  foo newtypenv env1
                                                            | _ -> failwith "tconstraints: error Val")
                                  | _ ->  tconstraintssem  (expr (applyenv(environmentsem, e1)) environmentsem) environment environmentsem))
  | Eint e1 -> (TInt, [])
  | Echar e1 -> (TChar,[])
  | True | False -> (TBool, [])
  | Empty -> (TList [newvar()], [])        
  | Sum   (e1,e2)| Diff  (e1,e2) | Times (e1,e2) ->
      let (t1,c1) = tconstraintssem  e1 environment environmentsem in
      let (t2,c2) = tconstraintssem  e2 environment environmentsem in
      let c = [(t1,TInt); (t2,TInt)] in
        (TInt, c @ c1 @ c2)
  |And (e1,e2)|Or (e1,e2) -> 
      let (t1,c1) = tconstraintssem  e1 environment environmentsem in
      let (t2,c2) = tconstraintssem  e2 environment environmentsem in
      let c = [(t1,TBool);(t2,TBool)] in
        (TBool, c @ c1 @ c2)
  |Not e1 ->
     let (t1,c1) = tconstraintssem  e1 environment environmentsem in
       (TBool, [(t1,TBool)]@c1)
  | Eq (e1,e2)->      
      let (t1,c1) = tconstraintssem  e1 environment environmentsem in
      let (t2,c2) = tconstraintssem  e2 environment environmentsem in
      let c = [(t1,t2);(t1,t1);(t2,t2)]  in
        (TBool, c @ c1 @ c2)
  |Less (e1,e2) ->
     let (t1,c1) = tconstraintssem  e1 environment environmentsem in
     let (t2,c2) = tconstraintssem  e2 environment environmentsem in
     let c = [(t1,TInt); (t2,TInt)] in
       (TBool, c @ c1 @ c2)
  | Cons(e1,e2) ->
      let (t1,c1) = tconstraintssem  e1 environment environmentsem in
      let (t2,c2) = tconstraintssem  e2 environment environmentsem in 
      let c =  [(t1,t1);(t2, TList [t1])] in
        (TList [t1], (c@c1@c2)) 
  | Head l -> 
      let a = newvar() in
      let (t1,c1) =tconstraintssem  l environment environmentsem in
        (match t1 with
             (TList [l1]) -> (l1, ([(t1, TList [l1])]@c1))
           | TVar n -> (a, ([(TList [a], t1)]@c1))
           | _ -> failwith "tconstraints: error head inference")             
  | Tail l ->
      let a = newvar() in
      let (t1,c1) = tconstraintssem  l environment environmentsem
      in (match t1 with
              TList [l1] -> (t1, (c1))
            | TVar n ->  (TList[t1], ([TList[a],t1]@c1))
            | _ -> failwith "tconstraints: error tail inference")                       
  |Epair (e1,e2) -> (*da rivedere se non funziona*)
     let (t1, c1) = tconstraintssem  e1 environment environmentsem in
     let (t2, c2) = tconstraintssem  e2 environment environmentsem in
     let c = [(t1, t1); (t2, t2 )] in
       (TPair(t1,t2), c@c1@c2)         
  | Fst e ->
      let a = newvar() in
      let (t1,c1) = tconstraintssem  e environment environmentsem
      in (match t1 with
              TPair(f,s) -> (f, ([t1, TPair(f,s)]@c1))
            | TVar n -> (a, [t1, TPair(a,newvar())]@c1)
            | _ -> failwith "tconstraints: error fst pair inference")           
  | Snd e ->
      let a = newvar() in
      let (t1, c1) = tconstraintssem  e environment environmentsem
      in (match t1 with
              TPair(f,s) -> (s, ([(t1, TPair(f,s))]@c1))
            | TVar n ->  (a, [t1, TPair(newvar(),a)]@c1)
            | _ -> failwith "tconstraints: error snd pair inference")               
  |Ifthenelse (e0,e1,e2) -> 
     let (t0,c0) = tconstraintssem  e0 environment environmentsem in
     let (t1,c1) = tconstraintssem  e1 environment environmentsem in
     let (t2,c2) = tconstraintssem  e2 environment environmentsem in
     let c = [(t0,TBool); (t1,t2)] in
       (t1, c @ c0 @ c1 @ c2)              
  | Let (x,e1,e2) -> let a = newvar() in
    let (t1,c1) = tconstraintssem  e1 environment environmentsem in
    let (t2,c2) = tconstraintssem  e2 (bindtyp environment x a) environmentsem in
      (t2, ([(t1, a)]@c1@c2))        
  | Fun(x,t) ->
      let a = newvar() in
      let (t1, c1) = tconstraintssem  t (bindtyp environment x a) environmentsem
      in (TFun(a,t1),c1)             
  | Appl(e1,e2) ->
      let a = newvar()
      in let (t1, c1) = tconstraintssem  e1 environment environmentsem
      in let (t2, c2) = tconstraintssem  e2 environment environmentsem
      in (a, ([t1, TFun(t2,a)]@c1@c2))           
  | Rec(y,f) -> let a = newvar() in
      (match f with
           Fun(x,t) -> let (t1, c1) =  tconstraintssem  f (bindtyp environment y a) environmentsem
           in (t1, ([a,a]@c1))
         | _ -> failwith "tconstraints: error Rec inference")
  | Try(e1,n,e2) -> 
      let (t2, c2) = tconstraintssem e2 environment environmentsem
      in let (t1, c1) = tconstraintssem e1 (bindtyp environment n t2) environmentsem
      in (t2, [t1, t2]@[])
  | Raise e1 ->(applytypenv environment e1, []);;



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


(*FUNZIONE DI SUPPORTO TYPEINF PER SEM*)
let rec typeinf_App e env envsem= 
let (e1,e2) = tconstraintssem  e env envsem in 
let u =  unify e2 in 
if u = [] then e1 else resolve e1 u 
and
 resolve e u = match u with
    [] -> e
   |(TVar x, t1)::s1 -> resolve (typeCheck e x t1) s1        
   |_-> failwith "typeinf_App: resolve - I can not infer"
and 
typeCheck e t1 t2 = match e with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar n -> if n = t1 then t2 else TVar n
  | TFun (t3,t4) -> TFun (typeCheck t3 t1 t2 , typeCheck t4 t1 t2)
  | TPair(t3,t4) -> TPair (typeCheck t3 t1 t2, typeCheck t4 t1 t2)
  | TList [l] -> TList [typeCheck l t1 t2]
  | _ -> failwith "typeinf_App: typecheck - error typesmatch";;


(*Funzione di supporto per Eq in Sem*)
let rec typeCheckEq (a,b) = match a,b with
      Undefined, Undefined -> true
  | Int _,Int _ -> true 
  | Bool _, Bool _ -> true 
  | Char _, Char _ -> true
  | List _, List _ -> true
  | Pair ((_ as a),(_ as b)), Pair ((_ as c),( _ as d) ) ->  typeCheckEq(a,c)&&typeCheckEq(b,d)
  |_-> failwith "typeCheckEq: error typesmatch";;


(*La funzione � volta al controllo dei tipi inferiti all'interno della funzione Sem*)
let rec typeCheckInf (a,b) = match a,b with   
      TInt, TInt  ->true
    | TBool, TBool -> true
    | TChar, TChar -> true
    | (TPair(a,b), TPair(c,d)) ->  typeCheckInf (a,c) &&  typeCheckInf (b,d)
    | (TFun(a,b), TFun(c,d)) ->  typeCheckInf (a,c) &&  typeCheckInf (b,d)
    | (TList [a], TList [b]) -> typeCheckInf (a,b)
    | (TVar _, _) | (_, TVar _) -> true
    | _ -> failwith "typeCheckInf: error typesmatch";;


(*Pila delle eccezioni*)
let newstack = ([]:(ide*exp)list);;

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
  | Try(e1,i,e2) ->  controllerFV (e1,d1,(controllerFV (e2,d1,d2)))
  | Raise t -> d2   
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

(*Interprete, del progetto con ambiente di inferenza collegato. Appoggio alla funzione sem*)
and  sem_App e r envType pila = match e with
   Eint n -> Int n
  | Echar c -> Char c  
  | Val x ->  applyenv (r,x)
  | Sum   (e1,e2) -> Int (evalInt e1 r envType pila  + evalInt e2 r envType pila ) 
  | Diff  (e1,e2) -> Int (evalInt e1 r envType pila  - evalInt e2 r envType pila )
  | Times (e1,e2) -> Int (evalInt e1 r envType pila* evalInt e2 r envType pila )
  | True  -> Bool true
  | False -> Bool false
  | Eq (e1,e2) -> (match sem_App e1 r envType pila, sem_App e2 r envType pila with
                       Int a, Int b   -> Bool (a=b) 
                     | Bool a, Bool b -> Bool (a=b) 
                     | Char a, Char b -> Bool (a=b) 
                     | List [], List [] -> Bool true
                     | List [a], List [] -> Bool false
                     | List [], List [b] -> Bool false
                     | List a, List b -> if a = b then Bool true else if
                        typeCheckInf  ((typeinf_App e1 envType r  ),(typeinf_App e2 envType r)) then Bool (a=b)
                       else failwith "semApp: error Eq list type"   
                     | Pair(a,b), Pair (c,d) -> if  (typeCheckEq (a,c) &&  typeCheckEq (b,d))
                       then  Bool (a=c&&b=d) else failwith "semApp: error Eq Pair type"
                     | Closure(a,b), Closure (c,d) -> failwith "equal: fail"
                     |Undefined, Undefined -> Bool (Undefined=Undefined)
                     |_-> raise TypeMismatch) 
  | Less (e1,e2) -> Bool (evalInt e1 r envType pila <= evalInt e2 r envType pila )  
  | Not ne -> Bool (not (evalBool ne r envType pila))   
  | And (e1,e2) -> Bool (evalBool e1 r envType pila && evalBool e2 r envType pila)  
  | Or (e1,e2) -> Bool (evalBool e1 r envType pila|| evalBool e2 r envType pila)  
  | Empty -> List []  
  | Head e1 -> (let a = sem_App e1 r envType pila in 
               let b = (match a with
                        List [] -> failwith "semApp: empty list" 
                      | List (hd::tl) -> hd  
                      | _ -> raise TypeMismatch)         
               in b )  
  |Tail e1 -> (let a = sem_App e1 r envType pila in 
               let b = (match a with
                           List [] -> failwith "semApp: empty list" 
                         | List (hd::tl) -> tl  
                     | _ -> raise TypeMismatch)         
              in List b )
  | Cons(e1,e2) -> (let lista = ( match sem_App e2 r envType pila with
                           (List l) -> List (sem_App e1 r envType pila::l)
                         |_ -> failwith "semApp: typesmatch in Cons")
          in let  (e3,e4)= try (true, typeinf_App e envType r) with _ -> (false, TVar "")
          in if e3 then lista else failwith "semApp: typesmatch in Cons"  )
  |Epair (e1,e2) -> Pair ( sem_App e1 r envType pila, sem_App e2 r envType pila) 
  |Fst e -> ( match (sem_App e r envType pila) with
                  Pair (a, b) -> a
                |_-> raise TypeMismatch) 
  |Snd e -> ( match (sem_App e r envType pila) with
                  Pair (a, b) -> b
                |_-> raise TypeMismatch) 
  | Ifthenelse(e0,e1,e2) -> 
      if evalBool e0 r envType pila then sem_App e1 r envType  pila else sem_App e2 r envType pila  
  | Let (x,e1,e2) -> sem_App e2 (bind (r,x,(sem_App e1 r envType pila))) 
      (bindtyp (envType) (x) (typeinf_App e1 envType r ) )  pila
  | Rec(y,(Fun(x,e1))) -> let n = (sub (e1,y,(Rec(y,Fun(x,e1))))) in
        Closure(Fun(x,n), controllerFV (n,r,emptyenv))   
  | Fun (x,e1) -> Closure ((Fun(x,e1)), controllerFV (e1,r,emptyenv))   
  | Appl (e1,e2) -> (match sem_App e1 r envType  pila with
                         Closure ((Fun(x,f)),d) -> 
                           (sem_App f (bind (d,x,(sem_App e2 r envType pila ))) 
                              (bindtyp envType x (typeinf_App e2 envType r ) ) pila )
                       | _ -> failwith "semApp: typesmatch in Closure")  
  | Try(e1,i,e2) -> sem_App e1 r (bindtyp envType i (typeinf_App e2 envType r)) ((i,e2)::pila)
  | Raise id -> let (n, s) = controllerTry id pila in sem_App n r envType s
  |_-> failwith "semApp: error typesmatch in guard" 

   
and controllerTry n s = match s with
    [] -> failwith "controllerTry: Empty stack exception"
  | (a,b)::tl -> if a = n then (b,tl) else controllerTry n tl;;



(*interprete principale del progetto*)
let rec semtry e r= sem_App e r newtypenv newstack;;
