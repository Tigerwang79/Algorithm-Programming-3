#use "ap3_def.ml";;
#use "mybool.ml";;
open MyBool;;
#use "mybtree.ml";;
open MyBtree;;

(* TP3 AP3 03/11 *)

(* Exercice 0 : Signature des Booléens *)

(* -- TEST À FAIRE -- *)
  
(* Exercice 1 : Signature des arbres binaires *)

(* -- TEST À FAIRE -- *)
  
(* Exercice 2 : Implantation avec un type somme *)

let n0 : int t_btree = empty();;
let n1 : int t_btree = rooting(7, empty(), empty());;
let n2 : int t_btree = rooting(6, n1, empty());;
let n3 : int t_btree = rooting(5, n2, empty());;
let n4 : int t_btree = rooting(4, n3, empty());;
let n5 : int t_btree = rooting(3, n4, empty());;
let n6 : int t_btree = rooting(2, n5, empty());;
let n7 : int t_btree = rooting(1, n6, empty());;

(* PS : Pas possible du fait de la non définition du show dans le module MyBtree :
show_int_btree(n7);;
show_int_btree(n6);;
show_int_btree(n5);;
show_int_btree(n4);;
show_int_btree(n3);;
show_int_btree(n2);;
show_int_btree(n1);;
*)

let rec size(btree : 'a t_btree): int =
  if isEmpty(btree)
  then 0
  else
    size(lson(btree))+size(rson(btree))+1
;;

size(n0);;
size(n1);;
size(n2);;
size(n3);;
size(n4);;
size(n5);;
size(n6);;
size(n7);;

let rec height(btree : 'a t_btree): int =
  if isEmpty(btree)
  then -1
  else
    max(height(lson(btree)),height(rson(btree)))+1
;;

height(n0);;
height(n1);;
height(n2);;
height(n3);;
height(n4);;
height(n5);;
height(n6);;
height(n7);;

let rec size_at_height(btree, height: 'a t_btree * int): int =
  if isEmpty(btree)
  then 0
  else
    if height = 0
    then 1
    else
      size_at_height(lson(btree), height-1)+size_at_height(rson(btree),height-1)
;;

size_at_height(n0,0);;
size_at_height(n1,0);;
size_at_height(n2,3);;
size_at_height(n3,2);;
size_at_height(n4,1);;
size_at_height(n5,5);;
size_at_height(n6,3);;
size_at_height(n7,6);;

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
intern_node(n0);;

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
leaf_btree(n0);;

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
left_border(n0);;

let rec right_border(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let r = root(btree) in
    r::right_border(rson(btree))
;;

right_border(n7);;
right_border(n6);;
right_border(n5);;
right_border(n4);;
right_border(n3);;
right_border(n2);;
right_border(n1);;
right_border(n0);;

let rec to_list_prefix(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_prefix(fg) in
    let l_fd = to_list_prefix(fd) in
    v::concat(l_fg,l_fd)
;;

let rec to_list_infix(btree: 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_infix(fg) in
    let l_fd = to_list_infix(fd) in
    concat(l_fg, concat([v],l_fd))
;;

let rec to_list_postfix(btree : 'a t_btree): 'a list =
  if isEmpty(btree)
  then []
  else
    let v,fg,fd = root(btree), lson(btree), rson(btree) in
    let l_fg = to_list_postfix(fg) in
    let l_fd = to_list_postfix(fd) in
    concat(l_fg, concat(l_fd,[v]))
;;

(* Exercice 3 : Implantation avec des pointeurs *)

(* -- TEST À FAIRE -- *)

(* Exercice 4 : Transposez en C *)
