(* Progetto gruppo con identificativo g03*)
(*#use evalg03.ml;;*)
(*Tipi exp, espressioni di dati astratti  eterogeni*)

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
| Rec of ide * exp               (* funzione ricorsiva*)
and 
ide = Ide of string;;


type etype =
  TBool
| TInt
| TVar of string
| TPair of etype * etype
| TList of etype list
| TFun of etype * etype;;




(* puntatore per nuova variabile con reference*)
let nextsym = ref (-1);;

(* creazione nuova variabile tramite il sistema puntatore*)

let newvar = fun () -> nextsym := !nextsym + 1 ;
  TVar ("?T" ^ string_of_int (!nextsym));;



(* vincoli, ci si prova*)



let rec vinc t = match t with
   Eint(n)->(TInt,[])
  |True->(TBool,[])
  |False->(TBool,[])
  |Sum(t1,t2)->(TInt, ([(t1,TInt)]@[(t2,TInt)]@(snd(vinc t1))@(snd(vinc t2))@[]))
  |Diff(t1,t2)->(TInt, ([(t1,TInt)]@[(t2,TInt)]@(snd(vinc t1))@(snd(vinc t2))@[]))
  |Times(t1,t2)->(TInt, ([(t1,TInt)]@[(t2,TInt)]@(snd(vinc t1))@(snd(vinc t2))@[]))
  |And(t1,t2)->(TBool, ([(t1,TInt)]@[(t2,TInt)]@(snd(vinc t1))@(snd(vinc t2))@[]))
  |Or(t1,t2)->(TBool, ([(t1,TInt)]@[(t2,TInt)]@(snd(vinc t1))@(snd(vinc t2))@[]))
  
  |_-> failwith "errore";;
