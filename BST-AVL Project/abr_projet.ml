#directory "4.08.1";;
#use "ap3_def.ml";;
#use "bst.ml";;
#load "btree.cmo";;
#show Btree;;
open Btree;;


(* Projet AP3 25/01 *)

(* Exercice 1 *)
(* Question 1 *)

Random.self_init();;

let rdm x =
  Random.int x
;;

let rec bst_rnd_create (x, tree : int * 'a t_btree): 'a t_btree =
  if x = 0
  then tree
  else bst_rnd_create(x-1, bst_linsert(rdm 100, tree))
;;


(* Question 2 *)

let max(a, b : int * int) : int =
  if(a > b)
  then a
  else b
;;

let rec height (abr : 'a t_btree) : int =
if isEmpty(abr)
then 0
else
  let h_fg : int = height(lson(abr))
  and h_fd : int = height(rson(abr)) in
  (1 + max(h_fg, h_fd))
;;

let rec dif(abr : 'a t_btree) : int =
  let a = height(rson(abr)) - height(lson(abr)) in
  if a > 0
  then a
  else -a
;;

let bst_generator(n : int) : int =
  dif(bst_rnd_create (n, empty()))
;;


let rec average_cal_bis(n, x : int * int) : int * int * int =
    if (x < 2) 
    then let a : int = bst_generator(n) in
       (a,a,a)
    else let tot,min,max = average_cal_bis(n,x-1) and
             a = bst_generator(n) in
         if(a < min)
         then (tot+a, a, max)
         else if(a > max)
         then (tot+a, min, a)
         else (tot+a , min, max)
;;


let average_cal (n, x : int * int) : float * int * int =
  let tot,min,max = average_cal_bis(n,x) in
  (float)tot/.(float)x,min,max

;;


(* Question 3*)

let rec ordo_list(list, elem : 'a list * int) =
  if list = []
  then [elem]
  else
    if List.hd list > elem
    then List.append [elem] list
    else List.append ([List.hd list]) (ordo_list(List.tl list, elem))
;;

let rec crea_list_ordo(x,n,list): 'a list =
  if (x<2)
  then
    let elem =rdm n in
    ordo_list(list, elem)
  else
    let elem =rdm n in
    ordo_list(crea_list_ordo(x-1,n, list), elem)
;;

(* Question 4 *)

(* Voir compte rendu d'experimentations *)


let rec crea_suite_list_aux(elem,n,list) =
  if n = 0
  then list
  else crea_suite_list_aux(elem+1, n-1, elem::list)
;;

crea_suite_list_aux(1,5,[]);;

let rec crea_suite_list(list, n) =
  if n =0
  then list
  else crea_suite_list(crea_suite_list_aux(Random.int 100, Random.int 10, list), n-1)
;;


let rec crea_bst_list(list, tree) =
  if list = []
  then tree
  else crea_bst_list(List.tl list, bst_linsert(List.hd list, tree))
;;
show_int_btree(crea_bst_list(crea_suite_list([], rdm 10), empty()));;


let bst_generator_list(n) =
  dif(crea_bst_list(crea_suite_list([], rdm n+1), empty()))
;;

let rec average_cal_bis_list(n, x : int * int) : int * int * int =
    if (x < 2) 
    then let a : int = bst_generator_list(n) in
       (a,a,a)
    else let tot,min,max = average_cal_bis_list(n,x-1) and
             a = bst_generator_list(n) in
         if(a < min)
         then (tot+a, a, max)
         else if(a > max)
         then (tot+a, min, a)
         else (tot+a , min, max)
;;

let average_cal_list(n, x : int * int) : float * int * int =
  let tot,min,max = average_cal_bis_list(n,x) in
  (float)tot/.(float)x,min,max

;;

(*
#trace average_cal_list;;
#trace average_cal_bis_list;;
#trace bst_generator;;
 *)

average_cal_list(100,1000);;
