#use "ap3_def.ml";;

(* TP3 AP3 03/11 *)

(* Exercice 1 : Signature des arbres binaires *)

module type Btree = 
  sig
    type 'a t_btree
    val empty : unit -> 'a t_btree
    val rooting : 'a * 'a t_btree * 'a t_btree -> 'a t_btree
    val root : 'a t_btree -> 'a
    val lson : 'a t_btree -> 'a t_btree 
    val rson : 'a t_btree -> 'a t_btree
    val isEmpty : 'a t_btree -> bool
    (*val show : ('a -> string) * 'a t_btree -> unit
    val show_int_btree : int t_btree -> unit
    val show_string_btree : string t_btree -> unit*)
  end
;;
    
(* Exercice 2 : Implantation avec un type somme *)

module MyBtree : Btree =
  struct
    type 'a t_btree = Empty | Node of 'a * 'a t_btree * 'a t_btree

    let empty() = Empty                                

    let rooting(v, lson, rson : 'a * 'a t_btree * 'a t_btree): 'a t_btree =
      Node(v, lson, rson)

    let root(abr : 'a t_btree): 'a =
      match abr with
        Empty -> failwith "Il n'y a pas de valeur au noeud."
        |Node(v, lson, rson) -> v

    let lson(abr : 'a t_btree): 'a t_btree =
      match abr with
        Empty -> failwith "Il n'y a pas de fils gauche."
        |Node(v, lson, rson) -> lson

    let rson(abr : 'a t_btree): 'a t_btree =
      match abr with
        Empty -> failwith "Il n'y a pas de fils droit."
        |Node(v, lson, rson) -> rson

    let isEmpty(abr : 'a t_btree): bool =
      match abr with
        Empty -> true
        |Node(v, lson, rson) -> false     
  end
;;
