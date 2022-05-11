#use "ap3_def.ml";;
#use "mybool.ml";;

(* TP3 AP3 03/11 *)

(* Exercice 3 : Implantation avec des pointeurs *)

module type BtreePtr = 
  sig
    type 'a t_btree
    val empty : unit -> 'a t_btree
    val rooting : 'a * 'a t_btree * 'a t_btree -> 'a t_btree
    val root : 'a t_btree -> 'a
    val lson : 'a t_btree -> 'a t_btree 
    val rson : 'a t_btree -> 'a t_btree
    val isEmpty : 'a t_btree -> bool
  end
;;


module MyBtreePtr : BtreePtr =
  struct
    type 'a t_btree = ('a t_node) ptr and
    'a t_node =
      {
        racine = 'a;
        lson = 'a t_btree;
        rson = 'a t_btree;
      }

    let empty() = null()

    let rooting(v, lson, rson : 'a * 'a t_btree * 'a t_btree): 'a t_btree =
      Node(v, lson, rson)

    let root(abr : 'a t_btree): 'a =
      if isEmpty(abr)
      then null()
      else Node(v, lson, rson) -> v

    let lson(abr : 'a t_btree): 'a t_btree =

    let rson(abr : 'a t_btree): 'a t_btree =

    let isEmpty(abr : 'a t_btree): bool =
      
  end
;;
