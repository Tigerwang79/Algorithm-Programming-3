#use "ap3_def.ml";;

(* TP3 AP3 03/11 *)

module type Bool =
  sig
    type bool
    val vrai : bool
    val faux : bool
    val neg : bool -> bool
    val ou : bool * bool -> bool
    val et : bool * bool -> bool
  end
;;

module MyBool : Bool =
  struct
    type bool = int
    let vrai = 1
    let faux = 0
    let neg(b : bool): bool =
      if b = 0
      then 1
      else 0
    let ou(b1, b2 : bool * bool) =
      if b1 = 1 || b2 = 1
      then 1
      else
        if b1 = 1 || b2 = 0
        then 1
        else
          if b1 = 0 || b2 = 1
          then 1
          else 0
    let et(b1, b2 : bool * bool): bool =
      if b1 = 1 && b2 = 1
      then 1
      else
        if b1 = 0 && b2 = 1
        then 0
        else
          if b1 = 1 && b2 = 0
          then 0
          else 0
  end
;;
