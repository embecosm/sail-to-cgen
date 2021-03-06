(*Generated by Lem from num_extra.lem.*)
open HolKernel Parse boolLib bossLib;
open lem_numTheory lem_stringTheory;

val _ = numLib.prefer_num();



val _ = new_theory "lem_num_extra"

(* **************************************************** *)
(*                                                      *)
(* A library of additional functions on numbers         *)
(*                                                      *)
(* **************************************************** *)

(*open import Num*)
(*open import String*)

(*val naturalOfString : string -> natural*)

(*val integerOfString : string -> integer*)


(* Truncation integer division (round toward zero) *)
(*val integerDiv_t: integer -> integer -> integer*)

(* Truncation modulo *)
(*val integerRem_t: integer -> integer -> integer*)

(* Flooring modulo *)
(*val integerRem_f: integer -> integer -> integer*)
val _ = export_theory()

