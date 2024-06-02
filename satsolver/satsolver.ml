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

(* Renvoie une formule construire à partir du contenu du fichier filename.
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
	assert (parse "(a > b) > c" = Or(Not (Or (Not (Var "a"), Var "b")), Var "c"));
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


(* Type sat_result qui permet de différencier le cas où la formule n'est pas satisfiable (None) ou l'est par la valusation sigma (Some(sigma)) *)
type sat_result = valuation option


(* SAT solver naif qui teste toutes les valuations possibles 
	Renvoie la première valuation qui satisfait f
	Renvoie None si f n'est pas satisfiable	*)
let sat_solver_naif (f: formule) : sat_result = 
	let rec aux_sat_solver_naif (f1: formule) (sigma: valuation option) : sat_result = 
		match sigma with 
		| None -> None
		| Some s -> if (interprete_f s f1 = true) then (Some s) else (aux_sat_solver_naif f1 (valuation_next s))
	in aux_sat_solver_naif f (Some (valuation_init (list_var f)));;
(* Tests *)
let test_sat_solver_naif () = 
	assert(sat_solver_naif (And (Var "a", Not (Var "a"))) = None);
	assert(sat_solver_naif (from_file "tests/test4.txt") = Some [("a",false);("b",false);("c",false);("d",false)]);
	assert(sat_solver_naif (from_file "tests/test5.txt") = None);
 	assert(sat_solver_naif (from_file "tests/test6.txt") = Some [("a",true);("b",false);("c",false)]);;

(* sipmle_step f renvoie un couple composé de f avec une étape de simplification suplémentaire
 (si c'est possible) et un booléen indiquant si une étape a été éffectuée *)
let rec simpl_step (f: formule): (formule * bool) = 
	match f with
	| And (Top, f1) | And (f1, Top) | Or (Bot, f1) | Or (f1, Bot) -> (f1, true)
	| And (Bot, f1) | And (f1, Bot) -> (Bot, true)
	| Or (Top, f1) | Or (f1, Top) -> (Top, true)
	| Not (Not (f1)) -> (f1, true)
	| Not (Top) -> (Bot, true)
	| Not (Bot) -> (Top, true)
	| And (f1, f2) -> let (f1s, b1) = simpl_step f1 in let (f2s, b2) = simpl_step f2 in (And(f1s, f2s), b1 || b2)
	| Or (f1, f2) -> let (f1s, b1) = simpl_step f1 in let (f2s, b2) = simpl_step f2 in (Or(f1s, f2s), b1 || b2)
	| Not (f1) -> let (f1s,b1) = simpl_step f1 in (Not f1s, b1)
	| f1 -> (f1, false)
(* Tests *)
let test_simpl_step () =
	assert(simpl_step(Not (Not (And (Var "a", Top)))) = (And (Var "a", Top), true));
	assert(simpl_step(And (Var "a", Top)) = (Var "a", true));
	assert(simpl_step(And(Var "a", Var "b")) = (And (Var "a", Var "b"), false));;

(* simpl_full f simplifie f au maximum (en appliquant simpl_step autant nécessaire) *)
let rec simpl_full (f: formule): formule =
	let (fs, b) = simpl_step f in 
	if b then simpl_full fs else fs
(* La même fonction mais en une compléxité linaire en la taille de la formule *)
let rec simpl_full_linear (f: formule): formule =
	(*print_int n;*)
	match f with
	| And (Bot, f1) | And (f1, Bot) -> Bot
	| Or (Top, f1) | Or (f1, Top) -> Top
	| Not (Top) -> Bot
	| Not (Bot) -> Top
	| And (Top, f1) | And (f1, Top) | Or (Bot, f1) | Or (f1, Bot) -> simpl_full_linear f1
	| Not (Not (f1)) -> simpl_full_linear f1
	| And (f1, f2) -> let f3 = And(simpl_full_linear f1, simpl_full_linear f2) in begin
			match f3 with 
			| And (Top, f4) | And (f4, Top) -> f4
			| And (Bot, f4) | And (f4, Bot) -> Bot
			| _ -> f3
		end
	| Or (f1, f2) -> let f3 = Or(simpl_full_linear f1, simpl_full_linear f2)in begin
			match f3 with
			| Or (Bot, f4) | Or (f4, Bot) -> f4
			| Or (Top, f4) | Or (f4, Top) -> Top
			| _ -> f3
		end
	| Not (f1) -> let f2 = Not(simpl_full_linear f1) in begin
			match f2 with
			| Not(Not (f3)) -> f3
			| Not(Top) -> Bot
			| Not(Bot) -> Top
			| f3 -> f3
	end
	| f1 -> f1
(* Tests *)
let test_simpl_full () =
	assert(simpl_full(Var "a") = Var "a");
	assert(simpl_full(And(Var "a", Not (Top))) = Bot);
	assert(simpl_full(And(Or (Var "a", Bot), And (Top, Var "b"))) = And (Var "a", Var "b"));;

(* subst f x g renvoie la formule f avec toutes les instances de x remplacées par g *)
let rec subst (f: formule) (x: string) (g: formule): formule =
	match f with
	| And (f1, f2) -> And (subst f1 x g, subst f2 x g)
	| Or (f1, f2) -> Or (subst f1 x g, subst f2 x g)
	| Not (f1) -> Not(subst f1 x g)
	| Var s -> if s = x then g else Var s
	| f1 -> f1 (* Bot ou Top -> ne change rien *)
(* Tests *)
let test_subst () =
	assert(subst (Var "a") "a" Top = Top);
	assert(subst (Or(Var "a",Var "b")) "a" (Var "b") = Or (Var "b", Var "b"));;

(* quine f renvoie un sat_result correspondant à la satisfiabilité de f grâce à l'algorithme de Quine *)
let quine (f: formule): sat_result =
	let fs = simpl_full f in
	let lv0 = list_var fs in
	let rec quine_reste (ff: formule) (lv: string list): sat_result =
		match lv with
		| [] -> begin 
						if ff = Top then Some([]) else None 
					end;
		| x::q -> 
			begin
				let r_top = quine_reste (simpl_full (subst ff x Top)) q in 
				match r_top with
				| Some (sat_result_list) -> Some((x, true)::sat_result_list)
				| None -> let r_bot = quine_reste (simpl_full (subst ff x Bot)) q in 
					begin 
						match r_bot with
						| Some (sat_result_list) -> Some((x, false)::sat_result_list)
						| None -> None
					end 
			end
	in quine_reste fs lv0
(* Tests *)
let test_quine () =
assert(quine (from_file "tests/test4.txt") = Some[("a", true); ("b", true); ("c", true); "d", true]);;

let quine_opt (f: formule): sat_result =
	let lv0 = list_var f in
	let rec quine_reste (ff: formule) (lv: string list) (valu: valuation): sat_result =
	let f_simpl = simpl_full_linear ff in 
	if f_simpl = Top then Some(valu) else 
		if f_simpl = Bot then None 
		else 
	match lv with
	| [] -> None 
	| x::q -> 
		begin
			let r_top = quine_reste (subst f_simpl x Top) q ((x,true)::valu) in 
			match r_top with
			| Some (sat_result_list) -> Some(sat_result_list)
			| None -> quine_reste (subst f_simpl x Bot) q ((x, false)::valu)
		end
	in quine_reste f lv0 []

(* type pour gérer les littéraux i.e. les variables et leur contraire (N) *)
type litteral =
	| Y of string
	| N of string

(* clause_D_list_var renvoie Some de la liste des littéraux de f si il n'y a pas de and, et None si il y en a *)
let rec clause_D_list_var (f : formule) (fg: formule) (clv_r: litteral list) : litteral list option =  
	match f with
	| Top | Or (Top, Top) -> 
	begin
		match fg with 
		| Top -> Some(clv_r)
		| _ -> clause_D_list_var fg Top clv_r
	end
	| Bot -> clause_D_list_var fg Top clv_r
	| Not (Var s) -> clause_D_list_var fg Top (N (s)::clv_r)
	| Var s -> clause_D_list_var fg Top (Y (s)::clv_r)
	| Not(_) -> None
	| And (f1, f2) -> None
	| Or (f1, f2) -> clause_D_list_var f2 (Or (fg, f1)) clv_r

(* fnc f renvoie None si f n'est pas sous FNC, et renvoie Some de la liste des clause conjonctive de f sous la forme d'une litteral list list si f est sous FNC*)
(* fg permet à la fonction d'être récurrsive terminale car : fnc (And (A, And (b,c))) = fnc (And (And (a,b), c)). Pour pouvoir rester linéaire en la taille totale 
	de l'arbre de la formule *)
(* Le bool détermine si fg est important (pour éviter une boucle infinie) *)
let rec fnc (f: formule) (fg : formule) (l_ll : litteral list list) (b: bool): litteral list list option =
	match f with
	| And (Top, f1) | And (f1, Top) -> fnc f1 Top l_ll false
	| And (f1, f2) -> fnc f2 (And (fg, f1)) l_ll true
	| Or (f1, f2) -> 
	begin 
		let clv = clause_D_list_var (Or (f1, f2)) Top [] in
		match clv with
		| None -> None
		| Some (lv) -> fnc fg Top (lv::l_ll) false
	end 
	| Var s -> fnc fg Top ([Y (s)]::l_ll) false
	| Not (Var s) -> fnc fg Top ([N (s)]::l_ll) false
	| Top -> if b then fnc fg Top l_ll false else Some (l_ll)
	| Bot -> fnc fg Top l_ll false
	| Not (f1) -> None

(* Paquet de fonction servant à l'étape de simplification de quine sous FNC (non utilisées ici car remplacées par simpl_toutes_clauses qui fait tout d'un coup 
		et donc qui gagne en complexité (en divisant le temps par ~2)) *)

(* Détermine si l est dans l_l *)
let rec est_dans_clause (l_l: litteral list) (l: litteral): bool =
	match l_l with
	| x::q -> if x = l then true else est_dans_clause q l
	| [] -> false

(* Supprime toutes les clause de l_ll contenant l *)
let rec suppr_clause (l_ll: litteral list list) (l: litteral) (l_ll_verifies: litteral list list): litteral list list = 
	match l_ll with
	| [] -> l_ll_verifies
	| l_l::q -> if est_dans_clause l_l l then suppr_clause q l l_ll_verifies else suppr_clause q l (l_l::l_ll_verifies)

(* Supprime l de l_l *)
let rec suppr_dans_clause (l_l: litteral list) (l: litteral) (l_l_verifies: litteral list): litteral list =
	match l_l with
	| [] -> l_l_verifies
	| x::q -> if x = l then suppr_dans_clause q l l_l_verifies else suppr_dans_clause q l (x::l_l_verifies)

(* Supprime l de chaque clause de l_ll *)
let rec suppr_de_clause (l_ll: litteral list list) (l : litteral) (l_ll_verifies: litteral list list): litteral list list =
	match l_ll with
	| [] -> l_ll_verifies
	| l_l::q -> suppr_de_clause q l (suppr_dans_clause l_l l []::l_ll_verifies)



(* Détermine si l contient une liste vide *)
let rec contient_clause_vide (l: 'a list list): bool =
	match l with 
	| []::q -> true
	| x::q -> contient_clause_vide q
 	| [] -> false


(* Fonction effectuant l'étape de simplification pour quine_fnc, consitant à supprimer chaque clause contenant l et chaque non l des clauses qui le contiennent
	où l_ll est la liste de liste de littéraux (cf quine), l le littéral à tester et l_ll_verifies un accumulateur composé de la liste qui a été testé et qui doit être gardée *)
let rec simpl_toutes_clause (l_ll: litteral list list) (l: litteral) (l_ll_verifies: litteral list list): litteral list list =
	match l_ll with 
	| [] -> l_ll_verifies 
	| l_l::q -> 
	begin
		(* Fonction auxiliaire qui effectue une simplification à une clause disjonctive de la formule en l'enlevant (None) si elle contient aux_l (le littéral à tester) 
			et enlevant le littéral non l si il apparaît *)
		let rec simpl_clause (aux_l_l: litteral list) (aux_l: litteral) (aux_l_l_verifies: litteral list): litteral list option =
		match aux_l_l with
		| [] -> Some (aux_l_l_verifies)
		| x::q -> 
		begin 
			match (x,l) with
			| (N s1, N s2) | (Y s1, Y s2) -> if s1 = s2 then None else simpl_clause q aux_l (x::aux_l_l_verifies) (* Si aux_l_l contient aux_l *)
			| (N s1, Y s2) | (Y s1, N s2) -> if s1 = s2 then simpl_clause q aux_l aux_l_l_verifies else simpl_clause q aux_l (x::aux_l_l_verifies) (* Si aux_l_l contient non aux_l *)
		end in let r_clause = simpl_clause l_l l [] in 
		match r_clause with 
		| None -> simpl_toutes_clause q l l_ll_verifies (* La clause a été retirée *)
		| Some (l_ll1) -> simpl_toutes_clause q l (l_ll1::l_ll_verifies) (* On rajoute la clause modifiée (ou non) à celles déjà vues *)
	end

(* Algorithme de quine pour les formules sous FNC, où lcd est une liste de liste de littéraux où chaque liste de littéraux représente une clause disjonctive
	de la formule à vérifier, llvv est la liste des variables de la formule et valu_r est un accumulateur pour se souvenir de la valuation testée *)
let rec quine_fnc (lcd: litteral list list) (llvv: string list) (valu_r: valuation): sat_result =
	if contient_clause_vide lcd then None else
	if lcd = [] then Some(valu_r) else
	match llvv with
	| [] -> None
	| s::q ->
	begin 
		let s_bot = quine_fnc (simpl_toutes_clause lcd (N (s)) []) q ((s, false)::valu_r) in 
		match s_bot with
		| Some (valuation) -> Some (valuation)
		| None -> quine_fnc (simpl_toutes_clause lcd (Y (s)) []) q ((s, true)::valu_r)
	end

(* Quine optimisé pour prendre en compte les formules sous FNC *)
let quine_opt_FNC (f: formule): sat_result =
	let lv0 = list_var f in
	let rec quine_reste (ff: formule) (lv: string list) (valu: valuation): sat_result =
	let f_simpl = simpl_full_linear ff in 
	if f_simpl = Top then Some(valu) else 
		if f_simpl = Bot then None 
		else 
	let f_fnc = fnc f Top [] false in
	match f_fnc with
	| Some(lcd) -> quine_fnc lcd lv []
	| None ->
	match lv with
	| [] -> None 
	| x::q -> 
		begin
			let r_bot = quine_reste (subst f_simpl x Bot) q ((x,false)::valu) in 
			match r_bot with
			| Some (sat_result_list) -> Some(sat_result_list)
			| None -> quine_reste (subst f_simpl x Top) q ((x, true)::valu)
		end
	in quine_reste f lv0 []

(* Fonction de test *)
let test () = 
	assert (1=1);
	test_from_file();
	test_parse();
	test_add_one();
 	test_interprete();
  	test_sat_solver_naif();
 	test_valuation_init();
 	test_valuation_next();
 	test_simpl_step();
 	test_simpl_full();
 	test_subst();
 	test_quine();
	print_string "Tous les tests ont réussi \n"

let main () = 
  if (Array.length Sys.argv < 2) then failwith "Veuillez rentrer un argument" else 
  if (Sys.argv.(1) = "test") then test () else begin
  (*print_string "Formule : "; print_string (read_file Sys.argv.(1)); print_string "\n" ;*)
  let sr = quine_opt_FNC (from_file Sys.argv.(1)) in
  match sr with
  | None -> print_string "La formule n'est pas satisfiable\n"
  | Some (sigma) -> print_string "La formule est satisfiable en assignat 1 aux variables suivantes et 0 aux autres :\n";
  	let rec print_var (sigma: valuation) =
  		match sigma with
  		| [] -> print_string ""
  		| (s, b)::q -> (if b then (print_string s; print_string "\n")) ; print_var q
  	in print_var sigma
   end

let _ = main ()
