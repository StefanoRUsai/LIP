(*#use typingg03.ml;;*)


(************************************************************)
(*                  ENVIRONMENT/AMBIENTE                    *)
(************************************************************)

(* "tipo valutazione dell'esprezzione" *)
(* il tipo evn è da rivedere / chiedere al professore*)

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
exception TypeMismatch ;;



let emptyenv = fun() -> Env(fun x -> Undefined)

and bind ((Env r), (Ide x), d) = Env (fun y -> if y=x then d else r y)

and applyenv ((Env r),(Ide x)) = match r x with
  Undefined -> raise (UndefinedIde (Ide x))
  | _ as d -> d;;

                
let rec evalInt e r = match (sem e r) with
  Int n -> n
| _ -> raise TypeMismatch
 
and evalBool e r = match sem e r with
  Bool b -> b
| _ -> raise TypeMismatch

and evalChar e r = match sem e r with
  Char c -> c
| _ -> raise TypeMismatch

     
and evalList e r = match sem e r with
  List c -> c
| _ -> raise TypeMismatch
     


and sem e r = match e with
  Eint n -> Int n
| Val x ->  applyenv (r,x)
| Sum (e1,e2) ->  Int (evalInt e1 r + evalInt e2 r)
| Diff (e1,e2) ->  Int (evalInt e1 r - evalInt e2 r)
| Times (e1,e2) ->  Int (evalInt e1 r * evalInt e2 r)
| True -> Bool true
| False -> Bool false
| Eq (e1,e2) ->
   if (typeinf e1 = typeinf e2)&&
      (typeinf e1 = typeinf (Eint 0)) then
     Bool (evalInt e1 r = evalInt e2 r) else
   if (typeinf e1 = typeinf e2)&&
      (typeinf e1 = typeinf (True)) then
     Bool (evalBool e1 r = evalBool e2 r) else
     Bool (evalChar e1 r = evalChar e2 r)
| Less (e1,e2) ->
   if (typeinf e1 = typeinf e2)&&
      (typeinf e1 = typeinf (Eint 0)) then
     Bool (evalInt e1 r <= evalInt e2 r) else
   if (typeinf e1 = typeinf e2)&&
      (typeinf e1 = typeinf (True)) then
     Bool (evalBool e1 r <= evalBool e2 r) else
     Bool (evalChar e1 r <= evalChar e2 r)
| Not ne -> Bool (not (evalBool ne r))
| And (e1,e2) -> Bool (evalBool e1 r && evalBool e2 r)
| Or (e1,e2) -> Bool (evalBool e1 r || evalBool e2 r)
|Empty -> List []
|Cons (e1, Tail e2) -> List (e1::e2)        
| Ifthenelse(e0,e1,e2) -> if evalBool e0 r then sem e1 r else sem e2 r
| Let (x,e1,e2) -> sem e2 (bind (r,x,(sem e1 r)))
(*| Rec (x,e1,e2) -> let rec r1 = Env(fun y -> applyenv (bind (r1, x, (sem e1 r1))) y) in sem e2 r1;;
| Fun (x,eq) -> Fun (x,e1,r);;
| Apply (e1,e2) -> match sem e1 r with
                     EFun (x,e',r') -> sem e' (bind r' x (sem e2 r))*)
| _ -> raise TypeMismatch 
;;
