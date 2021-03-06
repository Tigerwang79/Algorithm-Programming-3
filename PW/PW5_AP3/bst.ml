(* TP5 AP3 17/11 *)


(* Exercice 1 : *)


module type Bst =
  sig
    type 'a bst
    val empty : unit -> 'a bst
    val isEmpty : 'a bst -> bool
    val root : 'a bst -> 'a
    val rooting : 'a * 'a bst * 'a bst -> 'a bst
    val lson : 'a bst -> 'a bst
    val rson : 'a bst -> 'a bst
    val bst_seek : 'a bst * 'a -> bool
    val bst_linsert : 'a bst * 'a -> 'a bst
    val bst_lbuild : 'a list * 'a bst -> 'a bst
    val bst_max : 'a bst -> 'a
    val bst_delete_max : 'a bst -> 'a bst
    val bst_delete : 'a * 'a bst -> 'a bst
  end
;;
      
      

module MyBst : Bst =
  struct
    type 'a bst =
      | Empty
      | Node of 'a * 'a bst * 'a bst
         
    let empty() = Empty

    let isEmpty(tree : 'a bst) : bool =
      match tree with
        Empty -> true
       |Node(v, lson, rson) -> false

    let root(tree : 'a bst) : 'a =
      match tree with
      Empty -> failwith ("Il n'y a pas de valeur au noeud.")
      |Node(v, lson, rson) -> v

    let rooting(v, lson, rson : 'a * 'a bst * 'a bst) : 'a bst =
      Node(v, lson, rson)

    let lson(tree : 'a bst) : 'a bst =
      match tree with
       Empty -> failwith ("Il n'y a pas de fils gauche.")
       |Node(v, lson, rson) -> lson

    let rson(tree : 'a bst) : 'a bst =
      match tree with
        Empty -> failwith ("Il n'y a pas de fils droit.")
      |Node(v, lson, rson) -> rson
                            
    let rec bst_seek(tree, v : 'a bst * 'a) : bool =
      if(isEmpty(tree))
      then false
      else
        if(root(tree) = v)
        then true
        else
          if(root(tree) > v)
          then bst_seek(lson(tree), v)
          else bst_seek(rson(tree), v)                        
                              
    let rec bst_linsert(tree, v : 'a bst * 'a) : 'a bst = 
      if(isEmpty(tree))
      then rooting(v, empty(), empty())
      else
        if(root(tree) >= v)
        then rooting(root(tree), bst_linsert(lson(tree), v), rson(tree))
        else rooting(root(tree), lson(tree), bst_linsert(rson(tree), v))

     let rec bst_lbuild(l, t) =
         match l with
         [] -> t
         | hd::tl -> bst_lbuild(tl, bst_linsert(t, hd))

    let rec bst_max(a : 'a bst) : 'a =
      if isEmpty(a)
      then failwith "max non def"
      else
        if isEmpty(rson(a))
        then root(a)
        else bst_max(rson(a))

    let rec bst_delete_max(tree : 'a bst) : 'a bst =
      if isEmpty(tree)
      then failwith "arbre vide"
      else
        if isEmpty(rson(tree))
        then lson(tree)
      else rooting(root(tree), lson(tree), bst_delete_max(rson(tree)))

    let rec bst_delete(v, tree : 'a * 'a bst) : 'a bst =
      if isEmpty(tree)
      then empty()
      else
        let (r, ls, rs) = (root(tree), lson(tree), rson(tree)) in
        if r = v
        then
          let max = bst_max(ls) in
          let dl_max = bst_delete_max(ls) in
          rooting(max, dl_max, rs)
        else
          if r > v
          then rooting(r, bst_delete(v, ls), rs)
        else rooting(r, ls, bst_delete(v, rs))
  end
;;



















