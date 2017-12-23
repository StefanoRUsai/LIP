(*#use typingg03.ml;;*)


(************************************************************)
(*                  ENVIRONMENT/AMBIENTE                    *)
(************************************************************)

(* "tipo valutazione dell'esprezzione" *)
type eval =
  Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env
and
 env = Env of (string -> eval);;

  
exception UndefinedIde of ide;;
exception TypeMismatch of string;;

let emptyenv = fun()-> Env(fun x -> Undefined);;

(* da capire perchè non vede direttamente x come IDE*)

let bind ((Env r), (Ide x),d) = Env (fun y -> if y=x then d else r y);; 

let applyenv (r,x) = match r x with
  Undefined -> raise (UndefinedIde x)
| _ as d -> d ;;

let typechecker (x, y) = match x with
    | "int" -> (match y with 
	       |  Int(t) -> true
	       | _ -> false)
    | "bool" -> (match y with 
	        |  Bool(t) -> true
	        | _ -> false)
    | "char" -> (match y with
                | Char(t) -> true
                | _ -> false)
    | "eval list" -> (match y with
                | List(t) -> true
                | _ -> false)

    |"eval * eval" -> (match y with
                |Pair(a,b)-> true
                |_-> false)
    | "exp * env" -> (match y with 
                | Closure(a,b) -> true
                |_->false)       
    | _ -> failwith ("not a valid type");;



