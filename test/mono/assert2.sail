$include <smt.sail>
$include <flow.sail>
default Order dec
type bits ('n : Int) = vector('n, dec, bit)
val operator & = "and_bool" : (bool, bool) -> bool
val eq_vec = {ocaml: "eq_list", lem: "eq_vec"} : forall 'n. (bits('n), bits('n)) -> bool
overload operator == = {eq_int, eq_vec}
val mult_int = {ocaml: "mult", lem: "integerMult"} : (int, int) -> int
overload operator * = {mult_range, mult_int, mult_real}
val replicate_bits = "replicate_bits" : forall 'n 'm. (bits('n), atom('m)) -> bits('n * 'm)
overload operator < = {lt_atom, lt_int}

/* Should find a set constraint below the let */

val f : forall 'n. atom('n) -> unit effect {escape}

function f(n) = {
    let 'm = 2 * n in {
      assert(constraint('n in {8,16}));
      let x : bits('m) = replicate_bits(0b0,'m) in
      ()
    }
}


val run : unit -> unit effect {escape}

function run () = {
    f(8);
    f(16);
}