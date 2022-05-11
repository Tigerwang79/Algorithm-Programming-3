#use "ap3_def.ml";;


(* TP1 AP3 30/09 *)

(* Exercice 1 : *)

let norme(x, y, z : float * float * float) =
  sqrt(x**2.+.y**2.+.z**2.)
;;

let a = 3.;;
let b = 9.;;
let c = 6.;;

norme(a,b,c);;



(* Exercice 2 : *)

(* N°(1) *)
let milieu(ax, ay, bx, by : float * float * float * float) : float * float =
  (1./.2.*.(ax+.bx), 1./.2.*.(ay+.by))
;;

let ax = 2.;;
let ay = 4.;;
let bx = 3.;;
let by = 1.;;
milieu(ax,ay,bx,by);;


(* N°(2) *)
let mediatrice(ax, ay, bx, by : float * float * float * float) : float * float * float =
  (2.*.(bx-.ax),2.*.(by-.ay),(ax+.bx)*.(ax-.bx)+.(ay+.by)*.(ay-.by))
;;

let ax = 2.;;
let ay = 4.;;
let bx = 3.;;
let by = 1.;;
mediatrice(ax,ay,bx,by);;


(* Exercice 3 : *)

(* N°(1) *)
let signe(x : int): string =
  if (x == 0)
  then "nul"
  else if x>0
  then "positif"
  else if x<0
  then "negatif"
  else failwith("Error try again")
;;

signe(1);;
signe(-5);;
signe(0);;


(* N°(2) *)
let max2(x, y : int * int): int =
  if x>y
  then x
  else y
;;

let min2(x, y : int * int): int =
  if x<y
  then x
  else y
;;

(* N°(3) *)
let max3(x, y, z : int * int * int): int =
  max2(x, max2(y, z))
;;

let min3(x, y, z : int * int * int): int =
  min2(x, min2(y, z))
;;


let x = 1;;
let y = 2;;
let z = 3;;

max2(x, y);;
min2(x, y);;
max3(x, y, z);;
min3(x , y, z);;


(* Exercice 4 : *)

(* utilisation d'une définition locale *)
let longueur(ax, ay, bx, by: float * float * float * float): float =
  let ac : float = by-.ay in
  let bc : float = bx-.ax in
  let phytagore(a, b : float * float): float=
    sqrt(a**2.+.b**2.) in
  phytagore(ac, bc)
;;

longueur(5.,4.,5.,7.);;



(* Exercice 5 : *)


(* N°(1) *)
let  rec qui_suis_je (n : int) : int =
  if n = 0
  then 0
  else n + (qui_suis_je (n-1))
;;

(* Que fait la fonction suivante et quel est son type ? *)
(* C'est une fonction factorielle et elle est de type int *)


(* N°(2) *)

let rec somme_carre(n: float ): float =
  if n <= 0.
  then 0.
  else n**2.+.(somme_carre(n-.1.))
;;


somme_carre(5.);;

(* Exercice 6 : *)

(* N°(1) *)

let rec heron(x, n : float * int) : float =
  if n = 0
  then 1.
  else
    let h = heron(x , n-1) in
    1./.2.*.(h +. x/. h)
;;

(* N°(2) *)

let rec limite_aux(prec, actu, n, x, epsi : float * float * int * float * float ) : float =
  if abs_float(actu -. prec) < epsi
  then actu
  else limite_aux(actu, 1./.2.*.(actu +. x/. actu), n+1, x, epsi)
;;

let limite(x, epsi : float * float ) : float =
limite_aux(heron(x, 0), heron(x , 1), 1, x, epsi)
;;

#trace limite_aux;;

limite(10., 10.**(-8.));;

(* N°(3) *)


(* Exercice 7 : *)


(* Exercice 8 : *)

let lp : int list = [500; 100; 50; 10; 5];;

List.hd(lp);;

let rec change(n, lp, l : int * int list * int list) : int list =
  if n = 0
  then l
  else if n / List.hd = 1
  then change(n-List.hd(lp), List.tl(lp), [List.hd(lp)]@l)
  else l
;;

change(1280, lp, []);;

(* Exercice 9 : *)

let l = [("non",5); ("oui",3); ("blanc",5)];;

let a,b = List.hd(l);;

let rec recap(l, n1, n2, n3 : (string * int) list * int * int * int) =
  if l = []
  then (n1, n2, n3)
  else
    let (a,b) : string * int = List.hd(l) in
    if a = "oui"
    then recap(List.tl(l), n1+b, n2, n3)
    else if a = "non"
    then recap(List.tl(l), n1, n2+b, n3)
    else recap(List.tl(l), n1, n2, n3+b)
;;

#trace recap;;

let r = recap(l, 0, 0, 0);;

let max2(a,b) =
  if a > b
  then a
  else b
;;

let max3(a, b, c) =
  if a > b
  then max2(a,c)
  else if b > c
  then max2(b,a)
  else max2(b,c)
;;

let resultat(l: (string * int) list): int =
  let (r1, r2, r3) = recap(l, 0, 0, 0) in
  max3(r1,r2,r3)
;;

resultat(l);;

let listDep = [ [("oui",2);("oui",222);("non",23);("blanc",21)];
                [("non",15);("oui",36);("blanc",19);("blanc",21)];
                [("oui",98);("non",42);("blanc",12);("blanc",21);]
                [("non",5);("blanc",6);("oui",9);("oui",1)]
              ]
;;

let rec aplatit(l,l2) =
  match l with
  |[] -> l2
  |hd::tl -> match hd with
             |[] -> aplatit(tl, l2)
             |hd2::tl2 -> aplatit(l, l2@[hd2])
;;

aplatit(listDep, []);;