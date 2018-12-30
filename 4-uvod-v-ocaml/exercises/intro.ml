
(* ========== Exercise 1: Introduction to OCaml  ========== *)


(*----------------------------------------------------------------------------*]
 The function [penultimate_element] returns the second-to-last element of a
 list. If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec penultimate_element = function
  | [] -> failwith "kurac"
  | x :: [] -> failwith "kurac"
  | x :: y :: [] -> x
  | x :: xs -> penultimate_element xs


(*----------------------------------------------------------------------------*]
 The function [get k list] returns the [k]-th element in the list [list].
 Numbering (as usual) starts with 0. If [k] is negative, the function returns
 the first element.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k = function
  | [] -> failwith "kraateek"
  | x :: xs -> if k = 0 then x else get (k - 1) xs

(*----------------------------------------------------------------------------*]
 The function [double list] doubles the occurences of elements in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x :: xs -> x :: x :: double xs 

(*----------------------------------------------------------------------------*]
 The function [divide k list] divides the list into a pair of lists. The first
 list contains the first [k] elements of the list and the second contains the
 rest.
 When [k] is outside the bounds of [list], the appropriate list should be empty.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  let rec aux acc1 acc2 = function
    | [] -> (List.rev acc1, acc2)
    | x :: xs -> if k = 0 then aux (x :: acc1) xs [] else aux (x :: acc1) acc2 xs
    in
    aux [] [] list

(*----------------------------------------------------------------------------*]
 The function [delete k list] removes the [k]-th element of the list.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k = function
  | [] -> []
  | x :: xs -> if k = 0 then xs else x :: delete (k - 1) xs

(*----------------------------------------------------------------------------*]
 The function [slice i k list] returns the sub-list of [list] from the [i]-th
 up to (excluding) the [k]-th element. Suppose that [i] and [k] are fitting.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i j = function
  | [] -> []
  | x :: xs when i = 0 -> if j = 0 then [x] else x :: slice i (j - 1) xs
  | x :: xs -> slice (i - 1) (j - 1) xs 

(*----------------------------------------------------------------------------*]
 The function [insert x k list] inserts (not replaces) [x] into the list at the
 index [k].
 If [k] is outside of bounds of [list], insert the element at the beggining or
 the end instead.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert k i = function
  | [] -> [k]
  | x :: xs as t -> if i = 0 then k :: t else x :: insert k (i - 1) xs

(*----------------------------------------------------------------------------*]
 The function [rotate n list] rotates the list to the left by [n] places.
 Suppose that [n] is within the bounds of [list].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate k = function
  | [] -> []
  | x :: xs as t -> if k = 0 then t else rotate (k - 1) (xs @ [x]) 

(*----------------------------------------------------------------------------*]
 The function [remove x list] removes all occurrences of [x] in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x = function
  | [] -> []
  | y :: xs -> if y = x then remove x xs else y :: remove x xs 

(*----------------------------------------------------------------------------*]
 The function [is_palindrome] checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list = 
  List.rev list = list

(*----------------------------------------------------------------------------*]
 The function [max_on_components] returns a list with the maximum element
 of the two given lists at the each index.
 The lenght of the returned list should be equal to the shorter of the lists.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 =
  match (list1, list2) with
  | ([], list) -> []
  | (list, []) -> []
  | (x :: xs, y :: ys) -> if x > y then x :: (max_on_components xs ys) else y :: (max_on_components xs ys)

(*----------------------------------------------------------------------------*]
 The function [second_largest] returns the second largest value in the list.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)


let rec second_largest list =
  let rec aux = function
  | [] -> failwith "ha"
  | x :: [] -> x
  | x :: xs -> max x (aux xs)
  in aux (remove (aux list) list)