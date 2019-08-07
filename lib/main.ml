(**************************************************************************)
(*     Sail                                                               *)
(*                                                                        *)
(*  Copyright (c) 2013-2017                                               *)
(*    Kathyrn Gray                                                        *)
(*    Shaked Flur                                                         *)
(*    Stephen Kell                                                        *)
(*    Gabriel Kerneis                                                     *)
(*    Robert Norton-Wright                                                *)
(*    Christopher Pulte                                                   *)
(*    Peter Sewell                                                        *)
(*    Alasdair Armstrong                                                  *)
(*    Brian Campbell                                                      *)
(*    Thomas Bauereiss                                                    *)
(*    Anthony Fox                                                         *)
(*    Jon French                                                          *)
(*    Dominic Mulligan                                                    *)
(*    Stephen Kell                                                        *)
(*    Mark Wassell                                                        *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This software was developed by the University of Cambridge Computer   *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems  *)
(*  (REMS) project, funded by EPSRC grant EP/K008528/1.                   *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*     notice, this list of conditions and the following disclaimer.      *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*     notice, this list of conditions and the following disclaimer in    *)
(*     the documentation and/or other materials provided with the         *)
(*     distribution.                                                      *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''    *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED     *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A       *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR   *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,          *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT      *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF      *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND   *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,    *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT    *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF    *)
(*  SUCH DAMAGE.                                                          *)
(**************************************************************************)

open Elf_loader;;

let opt_file_arguments = ref ([] : string list)
let opt_raw_files = ref ([] : (string * Nat_big_num.num)  list)
let options = Arg.align [
    ( "-raw",
      Arg.String (fun s ->
      let l = Util.split_on_char '@' s in
      let (file, addr) = match l with
        | [fname;addr] -> (fname, Nat_big_num.of_string addr)
        | _ -> raise (Arg.Bad (s ^ " not of form <filename>@<addr>")) in
      opt_raw_files := (file, addr) :: !opt_raw_files),
    "<file@0xADDR> load a raw binary in memory at given address.")]

let usage_msg = "Sail OCaml RTS options:"

let () =
  Arg.parse options (fun s -> opt_file_arguments := !opt_file_arguments @ [s]) usage_msg

let rec load_raw_files = function
  | (file, addr) :: files -> begin
      let ic = open_in_bin file in
      let addr' = ref addr in
      try
        while true do
          let b = input_byte ic in
          Sail_lib.wram !addr' b;
          addr' := Nat_big_num.succ !addr';
        done
      with End_of_file -> ();
      load_raw_files files
    end
  | [] -> ()

let () =
  Random.self_init ();
  begin
    match !opt_file_arguments with
    | f :: _ -> load_elf f
    | _ -> ()
  end;
  load_raw_files !opt_raw_files;
  (* ocaml_backend.ml will append from here *)
