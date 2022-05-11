#directory "4.05.0";;
#use "ap3_def.ml";;
#load "btree.cmo";;
#show Btree;;
open Btree;;
#load "ap3stack.cmo";;
#show Stack;;
open Ap3stack;;


(* TP4 AP3 17/11 *)

(* Exercice 1 : *)

(* Question 1 *)

let rec create_list(n : int): int list =
  if n = 0
  then []
  else n::(create_list(n-1))
;;

create_list(100000000);;

(* Question 2 *)

open List;;

let rec create_list_rt_aux(n, l : int * int list) : int list =
  if (l=[])
  then l
  else create_list_rt_aux(n, l::n)
;;


let rec create_list_rt(n : int): int list =
  
;;


(* Exercice 2 : *)

let rec hanoi(n, start, inter, dest : int * int * int * int): (int * int) list =
  if n = 0
  then []
  else (hanoi(n-1, start, dest, inter))@[(start,dest)]@(hanoi(n-1,inter,start,dest))
;;

hanoi(3,1,2,3);;


let left_path(t, p : 'a t_btree * ('a t_btree) t_stack) : ('a t_btree) t_stack =
  let a : 'a t_btree ref = ref t and
      p' : ('a t_btree) t_stack ref = ref p in
      while not(Btree.isEmpty(!a)) do
        p':=push(!a,!p');
        a:=lson(!a);
      done;
      !p'
;;


let infixe_path(t : 'a t_btree): unit =
  let p : ('a t_btree) t_stack ref = ref left_path(t,t_stack.empty()) and
      a : 'a t_btree ref = ref Btree.empty() in
  while not (t_stack.is_empty(!p)) do
    a:=top(!p);
    processing(root(!a));
    if not (Btree.isEmpty(rson(!a)));
    then p:=left_path(rson(!a), depiler(!p));
    else p:=depiler(!p);
  done;
;;

let hanoi_iter() : (int * int) list =
;;


        




