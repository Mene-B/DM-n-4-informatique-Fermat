type formule =
	| Var of string
	| Top
	| Bot
	| And of formule * formule
	| Or of formule * formule
	| Not of formule

let implique (f1, f2) = Or(Not f1, f2)
let equivalence (f1, f2) = And(implique (f1, f2), implique (f2, f1))

(*** PARSER ***)

exception Erreur_syntaxe
exception Fichier_invalide

(* Symboles:
	'T' -> true
	'F' -> false
	'&' -> And
	'|' -> Or
	'~' -> Not
	'>' -> implication
	'=' -> equivalence
 *)

(* Détermine si c correspond à un opérateur binaire logique *)
let is_binop (c: char) : bool = match c with 
	| '&' |  '|' |  '>' |  '='  -> true
	| _ -> false 

(* Priorité de l'opérateur c. Permet de déterminer
	comment interpréter une formule sans parenthèses.
	Par exemple, "x&y|z" sera interprété comme "(x&y)|z"
	car & est plus prioritaire que | *)
let priority (c: char) : int = match c with
	| '&' -> 4
	| '|' -> 3
	| '=' -> 2
	| '>' -> 1
	| _ -> raise Erreur_syntaxe (* c n'est pas un opérateur *)

(* indice de l'opérateur le moins prioritaire parmis ceux
   qui ne sont pas entre parenthèses entre s.[i] et s.[j] 
   inclus *)
 let find_op_surface (s: string) (i: int) (j: int) : int =
 	(* 
 	   Renvoie l'indice de l'opérateur le moins prioritaire entre
 	   i et j, sachant que res est l'indice du meilleur opérateur
 	   entre i et k-1.
 	   paren_lvl: niveau d'imbrication actuel des parenthèses *)
 	let rec find_op_paren (k:int) (res:int) (paren_lvl: int) : int  =
 		if k=j+1 then res else
 		if s.[k] = '(' then find_op_paren (k+1) res (paren_lvl+1)
 		else if s.[k] = ')' then find_op_paren (k+1) res (paren_lvl-1) 

 		(* Le caractère lu est pris si l'on est hors des parenthèses,
 		   que le caractère est bien un opérateur, et qu'il est moins
 		   prioritaire que le meilleur résultat jusqu'ici *)
 		else if paren_lvl = 0 
 			 && is_binop s.[k] 
 			 && (res = -1 || priority s.[k] < priority s.[res]) 
 			 then find_op_paren (k+1) k (paren_lvl)
 		else find_op_paren (k+1) res (paren_lvl)
 	in find_op_paren i (-1) 0;;

(* Renvoie une formule construite à partir de la chaîne s.
   Lève une exception Erreur_syntaxe si la chaîne ne représente pas une formule valide. *)


let parse (s: string) : formule =
	let n = String.length s in
	(* construit une formule à partir de s[i..j] *)
	let rec parse_aux (i: int) (j:int) =
		if not (0 <= i && i < n && 0 <= j && j < n && i <= j ) then raise Erreur_syntaxe else
		if s.[i] = ' ' then parse_aux (i+1) j
		else if s.[j] = ' ' then parse_aux i (j-1)
		else let k = find_op_surface s i j in 
		if k = -1 then
			if s.[i] = '~' then 
				Not (parse_aux (i+1) j)
			else if s.[i] = '(' then
				begin 
					if (s.[j] != ')') then (print_int j; failwith "mauvais parenthésage") else
					parse_aux (i+1) (j-1)
				end
			else if (i = j && s.[i] = 'T') then Top
			else if (i = j && s.[i] = 'F') then Bot
			else Var(String.sub s i (j-i+1))

		else match s.[k] with
			| '&' -> And(parse_aux i (k-1), parse_aux (k+1) j)
			| '|' -> Or(parse_aux i (k-1), parse_aux (k+1) j)
			| '=' -> equivalence(parse_aux i (k-1), parse_aux (k+1) j)
			| '>' -> implique(parse_aux i (k-1), parse_aux (k+1) j)
			| _ -> raise Erreur_syntaxe
	in parse_aux 0 (String.length s -1)

(* Renvoie une formule construire à partir du contenu du fichier fn.
   Lève une exception Erreur_syntaxe si le contenu du fichier n'est pas une formule valide.
   Lève une exception Sys_error(message_erreur) si le nom du fichier n'est pas valide. *)
let from_file (filename: string) : formule = 
	(* concatène toutes les lignes de f en une seule chaîne *)
	let rec read_lines f = 
		try 
			let next_line = input_line f in
			let s = read_lines f in
			next_line ^ s
		with 
			| End_of_file -> ""
	in
	let f = open_in filename in 
	let s = read_lines f in
	parse s
(* Tests *)
let test_parse () =
	assert (parse "a | (b & ~c)" = Or(Var "a", And(Var "b", Not (Var "c"))));
	assert (parse "(a > b) > c" = Or(Not ( Or(Not (Var "a"), Var "b")), Var "c"));
	assert (parse "~(a | ~b) & (c | d)" = And(Not (Or(Var "a", Not (Var "b"))), Or(Var "c", Var "d")));
	assert (parse "~~~~~~a" = Not (Not (Not (Not (Not (Not (Var "a")))))));;


(* Fonction de test de la fonction from_file *)
let test_from_file () =
	assert (from_file "tests/test1.txt" = parse "(~((a & b) > (b | d))) | (a & e)");
	assert (from_file "tests/test2.txt" = parse "((((a | b) & (~c & d) > ((~(e | f) > (g & h) & i) | j))) | ((k & l) & m) > (~(~n | o) & p)) | (((q & r) & s) > ((~t | ~u) > (((~v | w) & x) | (y&z))))");
	assert (from_file "tests/test3.txt" = parse "(e & f) > (a | b) & (c |d)");;


(*Renvoie le contenu du fichier fn sous forme de string.
   Le fichier ne doit contenir qu'une seule ligne*) 
let read_file (fn : string) : string = 
  let ic = open_in fn in 
  let res = input_line ic in 
  close_in ic ; res 


(* Fonction qui renvoie le nombre d'opérateurs d'une formule *)
let rec compte_ops (f: formule): int =
	match f with
	| And (f1, f2) | Or (f1, f2) -> 1 + compte_ops f1 + compte_ops f2
	| Not f1 -> 1 + compte_ops f1
	| _ -> 0


(* Si l1 et l2 sont triées strictement, union l1 l2 est triée strictement et contient les éléments de l1 et l2*)
let rec union (l1 : 'a list) (l2 : 'a list) : 'a list = 
  match (l1, l2) with
  | ([], _) -> l2 
  | (_ , []) -> l1
  | (x1::q1,x2::q2) -> 
    if x1 < x2 then x1::(union q1 l2)
    else if x1 = x2 then x1::(union q1 q2)
    else x2::(union l1 q2)


(* Renvoie la liste des variables de f, sans doublons*)
let rec list_var (f : formule) : string list =  
  match f with
	| Top | Bot -> []
	| And (f1, f2) | Or (f1, f2) -> union (list_var f1) (list_var f2)
	| Not f1 -> list_var f1
	| Var s -> [s]


(* Fonction vérifiant si une liste est triée de manière strictement croissante i.e si elle est croissante et sans doublons *)
let rec trie_strict (l : 'a list) : bool = 
	match l with
	| _::[] | [] -> true
	| x::y::q -> if (x<y) then trie_strict(y::q) else false;;


(* Type valuation représenté par une liste de couples d'une variable (string) et de sa valeur dans la valuation (bool) *)
type valuation = (string*bool) list


(* Si l représente un nombre x en binaire, renvoie x+1 en binaire*)
let rec add_one (l : bool list) : bool list = 
  match l with 
	| [] -> [true]
	| x::q -> 
		if x = true then false::add_one q 
		else true::q
(* Tests *)
let test_add_one() = 
	assert(add_one [true;false;false;true] = [false;true;false;true]);
	assert(add_one [true;true;true] = [false;false;false;true]) 


(* Fonction intermédiaire qui renvoie la valeur de l avariable var dans la valuation s *)
let rec find_val (s: valuation) (var: string) : bool = 
	match s with 
	| x::q -> let (z, b) = x in
		if z = var then b else find_val q var
	| _ -> false;;


(* Fonction qui renvoie l'interpretation de la formule f dans la valuation s *)
let rec interprete_f (s: valuation) (f: formule) : bool = 
	match f with 
	| And (f1, f2) -> (interprete_f s f1) && (interprete_f s f2)
	| Or (f1, f2) -> (interprete_f s f1) || (interprete_f s f2)
	| Not f1 -> not (interprete_f s f1)
	| Top -> true 
	| Bot -> false 
	| Var str -> find_val s str;;
(* Tests *)
let test_interprete () = 
	assert (interprete_f [("a",false);("b",true);("d",true);("e",true)] (from_file "tests/test1.txt") = false);
	assert (interprete_f ([("a",true);("b",false);("d",false);("e",true)]) (from_file "tests/test1.txt") = true);
	assert (interprete_f ([("a",true);("b",true);("d",false);("e",false)]) (from_file "tests/test1.txt") = false);;


(* Renvoie la valuation suivant de v. Si v est la val max, renvoie None*)
let valuation_next (v : valuation) : valuation option= 
	let rec aux (v : valuation): valuation*bool = 
	match v with 
	| [] -> ([],true)
	| (s,b)::q -> 
	  if b = false then ((s,true)::q,false)
		else let (l,m) = aux q in ((s,false)::l ,m) in 
	let (l,b) = aux v in 
	if b = true then None 
	else Some l
(* Tests *)
let test_valuation_next() = 
	assert(valuation_next [("a",true);("b",true)] = None);
	assert(valuation_next [("a",true);("b",false)] = Some [("a",false);("b",true)]);
	assert(valuation_next [("a",false);("b",true)] = Some [("a",true);("b",true)])


(* Fonction qui renvoie la première valuation des variables de sl, donc celle où elles sont toutes à false *)
let valuation_init (sl: string list): valuation =
	List.map (fun (s: string) -> (s, false)) sl
(* Tests *)
let test_valuation_init () =
	assert (valuation_init ["a"; "b"; "c"] = [("a",false);("b",false);("c",false)]);;




(* Fonction de test *)
let test () = 
	assert (1=1);
	test_from_file();
	test_parse();
	test_add_one();
 	test_interprete();
 	test_valuation_init();
 	test_valuation_next();
	print_string "Tous les tests ont réussi \n"

let main () = 
  if (Array.length Sys.argv < 2) then failwith "Veuillez rentrer un argument\n" else 
  if (Sys.argv.(1) = "test") then test () else 
  print_string (read_file Sys.argv.(1))

let _ = main ()
