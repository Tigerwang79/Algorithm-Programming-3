#use "ap3_def.ml";;
#load "bst.cmo";;
open MyBst;;
open Random;;


(* TP5 AP3 01/12 *)

(* Question 1 *)

Random.self_init();;

let rdm x = Random.int x;;

let rec bst_rnd_create(x , tree : int * 'a bst) : 'a bst =
  if x = 0
  then tree
  else bst_rnd_create(x-1, bst_linsert(tree, rdm 100))
;;

let abr = bst_rnd_create(5,empty());;
root(abr);;


(* Question 2 *)

let max(a , b : int * int) : int =
  if(a > b)
  then a
  else b
;;

max(5, 10);;


let rec height(tree : 'a bst) : int =
  if isEmpty(tree)
  then 0
  else
    let h_fg : int = height(lson(tree)) and
        h_fd : int = height(rson(tree)) in
    (1 + max(h_fg, h_fd))
;;

height(abr);;

let rec diff(tree : 'a bst) : int =
  height(rson(tree)) - height(lson(tree))
;;

diff(abr);;

let rec des_generator(n, e, d, c : int * int * int * int) : (int * int) =
  if n = 0
  then (d, c)
  else
    let abr = diff(bst_rnd_create(e, empty())) in
    des_generator(n-1, e, abr + d, c + 1)
;;

des_generator(10, 100, 0, 0);;

let rec average_cal_bis(n, e, d, c : int * int * int * int) : float =
  let res = des_generator(n, e, d, c) in
  
;;

let average_cal (n, x : int * int) : float * int * int =
  let tot,min,max = average_cal_bis(n,x) in
  (float)tot/.(float)x,min,max

;;


(* Question 3 A *)

