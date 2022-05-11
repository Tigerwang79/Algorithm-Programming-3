#directory "4.08.1";;
#use "ap3_def.ml";;
#use "bst.ml";;
#load "btree.cmo";;
#show Btree;;
open Btree;;

(* Projet AP3 25/01 *)

(* Question 1 *)

let rt_gauche (v, l, r) = 
  let (rv, rl, rr) = (root(r), lson(r), rson(r)) in
  rooting(rv, rooting(v, l, rl), rr)
;;

let rt_droite (v, l, r) = 
  let (lv, ll, lr) = (root(l), lson(l), rson(l)) in
  rooting(lv, ll, rooting(v, lr, r))
;;

let rt_gauche_droite (v, l, r) =
  let (lv, ll, lr) = (root(l), lson(l), rson(l)) in
  rt_droite( v, rt_gauche(lv, ll, lr), r )
;;

let rt_droite_gauche (v, l, r) =
  let (rv, rl, rr) = (root(r), lson(r), rson(r)) in
  rt_gauche(v, l, rt_droite(rv, rl, rr))
;;


(* Question 2 *)

let rec height (abr : 'a t_btree) : int =
if isEmpty(abr)
then 0
else
  let h_fg : int = height(lson(abr))
  and h_fd : int = height(rson(abr)) in
  (1 + max(h_fg, h_fd))
;;

let avl_dif(abr : 'a t_btree) : int =
  let a : int  = height(lson(abr)) - height(rson(abr)) in
  a
;;


let reequilibrer abr =
  match avl_dif abr with
  |2 -> if(avl_dif (lson abr) = 1)
        then rt_droite(root abr ,lson abr ,rson abr )
        else
          if( avl_dif (lson abr) = -1 )
          then rt_gauche_droite(root abr , lson abr , rson abr)
          else abr
  |(-2)->if(avl_dif (rson abr) = -1)
         then rt_gauche (root abr,lson abr,rson abr)
         else
           if ( avl_dif (rson abr)= 1)
           then rt_droite_gauche (root abr, lson abr,rson abr)
           else abr
  |_ -> abr
;;


(* Question 3 *)

let rec insertion(value, avl : 'a * 'a t_btree) : 'a t_btree =
  if isEmpty(avl)
  then rooting(value, empty(), empty())
  else
    let (v, l, r) = root(avl), lson(avl), rson(avl) in
    if value = v
    then avl
    else
      if value < v
      then reequilibrer(rooting(v, insertion(value, l), r))
      else reequilibrer(rooting(v, l, insertion(value, r)))
;;


(* Question 4 *)

let rec bst_seek(value, avl : 'a * 'a t_btree) : bool =
  if(isEmpty(avl))
  then false
  else
    if(root(avl) = value)
    then true
    else
      if(root(avl) > value)
      then bst_seek(value, lson(avl))
      else bst_seek(value, rson(avl))
;;

(* Exercice 2.2 *)


(* Question 1 *)

let rec avl_rnd_create(avl, x) =
  if x = 0
  then avl
  else avl_rnd_create(insertion(Random.int 100,avl), x-1)
;;
