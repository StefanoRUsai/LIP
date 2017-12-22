(* Progetto gruppo con identificativo g03*)
(*#use eval###.ml;;*)
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





(*Environment/Ambiente *)
  
exception UndefinedIde of ide;;
exception TypeMismatch of string;;

let emptyenv = fun()-> Env(fun x -> Undefined);;

(* da capire perchè non vede direttamente x come IDE*)
let bind ((Env r), (Ide x),d) = Env (fun y -> if y=x then d else r y);; 

let applyenv (r,x) = match r x with
  Undefined -> raise (UndefinedIde x)
| _ as d -> d ;;
