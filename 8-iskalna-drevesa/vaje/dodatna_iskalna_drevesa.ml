(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type option = 
  | None
  | Some of int 

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree


(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)
let leaf x = Node(Empty, x, Empty)

let test_tree = 
  let left_t = Node(leaf 0, 2, Empty) in
  let right_t = Node(leaf 6, 7, leaf 11) in
  Node(left_t, 5 , right_t)

let rec member x = function
		| Empty -> false
		| Node (lt, y, rt) -> if x = y then true else member x lt || member x rt

let rec insert x tree =
	if not(member x tree) then
		match tree with
		| Empty -> leaf x
		| Node(lt, y, rt) -> if x > y then Node(lt, y, insert x rt) else Node(insert x lt, y, rt)
	else
    tree
    
let rec bst_of_list listt =  
  let rec aux acc = function
    | [] -> acc 
    | x :: xs -> aux (insert x acc) xs
  in aux Empty listt

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)
 
let rec tree_sort list = 
  let rec aux = function
    | Empty -> []
    | Node(Empty, x, ys) -> x :: aux ys
    | Node(xs, x, Empty) -> aux xs @ [x] 
    | Node(xs, x, ys) -> aux xs @ [x] @ aux ys 
  in aux (bst_of_list list)

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)
type option = 
    | Left
    | Right

let rec follow sez list =
  match sez, list with
  | _, Empty -> None
  | [], Node(xs, x, ys) -> Some x
  | a :: xt, Node(xs, x, ys) -> if a = Left then follow xt xs else follow xt ys  

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune dir tre =
  match dir, tre with
  | _, Empty -> Empty
  | [], Node(_, y, _) -> Empty
  | a :: xt, Node(xs, x, ys) -> if a = Left then Node(prune xt xs, x ,ys) else Node(xs, x, prune xt ys) 


  
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type state = 
  | Exists
  | Ghost

type 'a ptree =
  | P_Empty
  | P_Node of 'a ptree * 'a * 'a ptree * state

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)
let rec phantomize = function
  | Empty -> P_Empty
  | Node(xs, y, ys) -> P_Node(phantomize xs, y, phantomize ys, Exists)

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)
let rec to_sez = function
  | P_Empty -> []
  | P_Node(x, y, ys, a) -> if a = Ghost then (to_sez x) @ to_sez ys else (to_sez x) @ [y] @ to_sez ys

let rec unphantomize tree =
  let rec aux = function
    | list -> bst_of_list list
  in aux (to_sez tree)