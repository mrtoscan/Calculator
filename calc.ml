type tree = 
    Tree of tree * op * tree 
  | Value of float

and op = float -> float -> float

type associativity = Left | Right

let rec eval = function
    Tree (l, op, r) -> op (eval l) (eval r)
  | Value e -> e

let to_op = function
  | '+' -> ( +. )
  | '-' -> ( -. )
  | '*' -> ( *. )
  | '/' -> ( /. )
  | '^' -> ( ** )
  | _   -> failwith "opérateur inconnu"

let op_list = [('+', Left); ('-', Left); ('*', Left); ('/', Left); ('^', Right)] (* Les opérateurs et leurs associativités (par défaut à gauche) *)

(* Dans l'ordre, on regarde si l'expression est de la forme :
    - (expression) (1)
    - expression op expression (2)
    - nombre (3)
 *)
let rec analyze expr =
  let rec iter' = function (* Retourne None si expr n'est pas de la forme (2), retourne un arbre sinon. *)
    | [] -> None
    | x::xs -> let result = find_op expr x in
               if result = None then iter' xs else result in
  let p = pth expr in (* On teste d'abord (1) *)
  if p = None then
    let result = iter' op_list in (* Si (1) a échoué, teste (2) (il y a plusieurs opérateurs à tester par ordre de précédence) *)
    if result = None then number expr (* Si (3) a échoué, ne reste plus que (3) ! *)
    else result   
  else p

and pth expr = 
  try 
    if expr.[0] != '(' || expr.[(String.length expr)-1] != ')' then None
    else analyze (String.sub expr 1 ((String.length expr)-2)) (* Si expr est de la forme (expr') on analyse récursivement sur expr' *)
  with Invalid_argument "String.sub" -> None 

and find_op expr (opt, a) =
  try
    let rec aux expr (opt, a) fol =
	  (* on cherche la première occurrence de opt à partir de l'indice fol si opt est associatif à gauche
         et on cherche la dernière avant fol si opt est associatif à droite. *)
      let i = (if a = Right then String.index_from expr fol opt else String.rindex_from expr fol opt) in
      let left = String.sub expr 0 i in (* on découpe la chaine à gauche et à droite *)
      let right = String.sub expr (i+1) ((String.length expr)-(i+1)) in
      match (analyze left, analyze right) with (* on analyse récursivement sur left et right *)
        | (None, _) -> aux expr (opt,a) (if a = Right then i+1 else i-1) (* si on a obtenu None pour left ou right, on est peut-être juste entré dans une parenthèse sans le vouloir, 
                                                                           donc on teste la prochaine occurrence *)
        | (_, None) -> aux expr (opt,a) (if a = Right then i+1 else i-1)
        (* mais si on a réussi à obtenir un arbre des deux côtés, on peut construire le nœud supérieur *)
        | (Some u, Some v) -> Some (Tree (u, (to_op opt), v))
    in aux expr (opt,a) (if a = Right then 0 else (String.length expr)-1)
  with (* Si une des exceptions est levée, en particulier Not_found, expr n'est pas de la forme expr' opt expr' *)
    Not_found -> None
  | Invalid_argument "String.index_from" -> None
  | Invalid_argument "String.sub" -> None

and number expr = (* on finit par tester si expr est juste un nombre *)
  try Some (Value (float_of_string expr)) with Failure "float_of_string" -> None

let () =
  let remove_blanks = Str.global_replace (Str.regexp " +") "" in (* on enlèvera les espaces dès le début *)
  let s = read_line () in
  match analyze (remove_blanks s) with
    | None -> print_string "Expression invalide." (* on a pas réussi à construire un arbre, l'expression donnée n'est pas valide *)
    | Some tree -> print_float (eval tree) 