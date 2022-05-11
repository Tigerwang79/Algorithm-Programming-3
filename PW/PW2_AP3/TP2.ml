#directory "4.05.0";;
#use "ap3_def.ml";;
#load "btree.cmo";;
(*#load "gtree.cmo";;*)
#show Btree;;
(*#show Gtree;;*)
open Btree;;
(*open Gtree;;*)


(* TP2 AP3 06/10 *)


(* Exercice 1 : *)


(* N°(1) *)

let n1 : int t_btree = rooting(7, empty(), empty());;
let n2 : int t_btree = rooting(6, n1, empty());;
let n3 : int t_btree = rooting(5, n2, empty());;
let n4 : int t_btree = rooting(4, n3, empty());;
let n5 : int t_btree = rooting(3, n4, empty());;
let n6 : int t_btree = rooting(2, n5, empty());;
let n7 : int t_btree = rooting(1, n6, empty());;


(* N°(2) *)

let rec size(btree : 'a t_btree): int =
  if isEmpty(btree)
  then 0
  else
    size(lson(btree))+size(rson(btree))+1
;;

size(n1);;
size(n2);;
size(n3);;
size(n4);;
size(n5);;
size(n6);;
size(n7);;

(* N°(3) *)

let rec height(btree : 'a t_btree): int =
  if isEmpty(btree)
  then -1
  else
    max(height(lson(btree)),height(rson(btree)))+1
;;

height(n1);;
height(n2);;
height(n3);;
height(n4);;
height(n5);;
height(n6);;
height(n7);;


(* N°(4) *)

let rec size_at_height(btree, height: 'a t_btree * int): int =
  if isEmpty(btree)
  then 0
  else
    if height = 0
    then 1
    else
      size_at_height(lson(btree), height-1)+size_at_height(rson(btree),height-1)
;;

size_at_height(n1,0);;
size_at_height(n2,3);;
size_at_height(n3,2);;
size_at_height(n4,1);;
size_at_height(n5,5);;
size_at_height(n6,3);;
size_at_height(n7,6);;


(* N°(5) *)

let rec intern_node(btree : 'a t_btree): int =
  if isEmpty(btree)
  then 0
  else
    if isEmpty(lson(btree)) && isEmpty(rson(btree))
    then 0
    else
      intern_node(lson(btree))+intern_node(rson(btree))+1
;;

intern_node(n7);;
intern_node(n6);;
intern_node(n5);;
intern_node(n4);;
intern_node(n3);;
intern_node(n2);;
intern_node(n1);;


(* N°(6) *)

let rec leaf_btree(btree : 'a t_btree): int =
  if isEmpty(btree)
  then 0
  else
    if isEmpty(lson(btree)) && isEmpty(rson(btree))
    then 1
    else
      leaf_btree(lson(btree))+leaf_btree(rson(btree))
;;

leaf_btree(n7);;
leaf_btree(n6);;
leaf_btree(n5);;
leaf_btree(n4);;
leaf_btree(n3);;
leaf_btree(n2);;
leaf_btree(n1);;

show_int_btree(n7);;
show_int_btree(n6);;
show_int_btree(n5);;
show_int_btree(n4);;
show_int_btree(n3);;
show_int_btree(n2);;
show_int_btree(n1);;


(* Exercice 2 : Parcours sur les arbres binaires *)


(* N°(1) *)

let rec left_border(btree : 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let r = root(btree) in
    r::left_border(lson(btree))
;;

left_border(n7);;
left_border(n6);;
left_border(n5);;
left_border(n4);;
left_border(n3);;
left_border(n2);;
left_border(n1);;

let r0 : int t_btree = empty();;
let r1 : int t_btree = rooting(120, empty(), empty());;
let r2 : int t_btree = rooting(100, empty(), r1);;
let r3 : int t_btree = rooting(80, empty(), r2);;
let r4 : int t_btree = rooting(60, empty(), r3);;
let r5 : int t_btree = rooting(40, empty(), r4);;
let r6 : int t_btree = rooting(20, empty(), r5);;
let r7 : int t_btree = rooting(0, empty(), r6);;

show_int_btree(r0);;
show_int_btree(r1);;
show_int_btree(r2);;
show_int_btree(r3);;
show_int_btree(r4);;
show_int_btree(r5);;
show_int_btree(r6);;
show_int_btree(r7);;


let rec right_border(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let r = root(btree) in
    r::right_border(rson(btree))
;;

right_border(r7);;
right_border(r6);;
right_border(r5);;
right_border(r4);;
right_border(r3);;
right_border(r2);;
right_border(r1);;
right_border(r0);;


(* N°(2) *)

let rec to_list_prefix(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_prefix(fg) in
    let l_fd = to_list_prefix(fd) in
    v::concat(l_fg,l_fd)
;;

(*let rec to_list_infix(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_postfix(fg) in
    let l_fd = to_list_postfix(fd) in
    concat(l_fg, concat(l_fd,[v]))
;;*)


let rec to_list_postfix(btree : 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_postfix(fg) in
    let l_fd = to_list_postfix(fd) in
    concat(l_fg, concat(l_fd,[v]))
;;


















(*

(* N°(2) *)

let rec list_post_of_ab(btree : 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else let v,fg,fd = root(btree), lson(bt), rson(btree) in
       let l_fg = list_post_of_ab(fg)
       and l_fd = list_post_of_ab(fd) in
       append(concat(l_fg,l_fd),v)
;;


(* concat(l_fg, concat(l_fd, [v]))*)

 *)
