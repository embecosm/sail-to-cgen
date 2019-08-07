open Ast
open Ast_util
open PPrint

(* out_name = "/home/mary/Documents/SAIL/riscv.cpu"
   Useful files: riscv_insts_base.sail : instruction definitions
   riscv_types.sail      : registers *)

(* Describe information required by hardware *)
type hardware = string * string * (string list)

(* Iterator pg90 *)
let rec print_iter out_channel l =
  match l with
    | [] -> ()
    | h::t -> output_string out_channel h;
              print_iter out_channel t

(* Print indices *)
let print_indices out_channel l =
    print_iter out_channel l

(* Prints the define-hardware function *)
let define_hardware out_channel (name, hw_type, indices) =
  output_string out_channel "(define-hardware\n";
  output_string out_channel "  (name h-";
  output_string out_channel name;
  output_string out_channel ")\n";
  output_string out_channel "  (comment ";
  output_string out_channel name;
  output_string out_channel ")\n";
  output_string out_channel "  (attrs all-isas all-machs)\n";
  output_string out_channel "  (type ";
  output_string out_channel hw_type;
  output_string out_channel ")\n";
  match indices with
    | [] -> output_string out_channel ")\n"
    | h::t ->
      output_string out_channel "  (indices ";
      print_indices out_channel indices;
      output_string out_channel ")\n)\n"

let do_mapdef_registers out_channel (MD_aux (MD_mapping (_, _, clauses), _)) =
  output_string out_channel ("Mapping has " ^ string_of_int (List.length clauses) ^ " clauses");
  output_string out_channel "\n"

let print_hardware out_channel =
  let indices = ["(x0 0)"; "(x1 1)"; "(x2 2)"] in
    let hardware = ("name", "type", indices) in
      define_hardware out_channel hardware

(*let rec list_registers out_channel = function
  | [] -> ()
  | (DEF_reg_dec reg) :: defs ->
     print_string (Pretty_print_sail.to_string (Pretty_print_sail.doc_dec reg));
     print_newline;
     print_hardware out_channel;
     list_registers out_channel defs
  | (DEF_mapdef mapdef) :: defs ->
     do_mapdef_registers out_channel mapdef;
     list_registers out_channel defs
  | def :: defs ->
     list_registers out_channel defs*)

(* Called in sail.ml *)
let create_file out_name (Defs defs) =
  let ochannel = open_out out_name in
    try
    (*list_registers ochannel defs;*)
      print_hardware ochannel;
      close_out ochannel
    with
      _ -> close_out ochannel
