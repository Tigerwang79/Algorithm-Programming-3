#directory "4.05.0";;
#load "btree.cmo";;
open Btree;;
#show Btree;;

let rec bst_seek(abr, n: 'a t_btree * 'a): bool =
  if(isEmpty(abr))
  then false
  else
    if(root(abr)=n)
    then true
    else
      if(root(abr)>n)
      then bst_seek(lson(abr),n)
      else bst_seek(rson(abr),n)
;;

let rec bst_linsert(elem, tree : 'a * 'a t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then rooting(elem, empty(), empty())
  else
    if(elem < root(tree))
    then rooting(root(tree), bst_linsert(elem, lson(tree)), rson(tree))
    else rooting(root(tree), lson(tree), bst_linsert(elem, rson(tree)))
;;

let rec bst_delete_bis(abr : 'a t_btree):'a * 'a t_btree=
  if(isEmpty(rson(abr)))
  then (root(abr), lson(abr))
  else
    let (a,arbre):'a * 'a t_btree = bst_delete_bis(rson(abr)) in
    (a,rooting(root(abr), lson(abr),arbre))
;;

let rec bst_delete_ter(abr : 'a t_btree):'a * 'a t_btree=
  if(isEmpty(lson(abr)))
  then (root(abr), rson(abr))
  else
    let (a,arbre):'a * 'a t_btree = bst_delete_bis(lson(abr)) in
    (a,rooting(root(abr), arbre, rson(abr)))
;;


let rec bst_delete(abr, n: 'a t_btree * 'a):'a t_btree =
  if(isEmpty(abr))
  then empty()
  else
    if(root(abr)=n)
    then
      if(isEmpty(lson(abr)) && isEmpty(rson(abr)))
      then empty()
      else
        if(isEmpty(lson(abr)))
        then
          let (a,arbre):'a * 'a t_btree = bst_delete_ter(rson(abr)) in
          rooting(a,lson(abr),arbre)
        else
          let (a,arbre):'a * 'a t_btree = bst_delete_bis(lson(abr)) in
          rooting(a,arbre,rson(abr))
    else
      if(root(abr)>n)
      then rooting(root(abr),bst_delete(lson(abr),n),rson(abr))
      else rooting(root(abr),lson(abr),bst_delete(rson(abr),n))
;;
