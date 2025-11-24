open Gospel
open Parser_frontend
open Uattr2spec

let fname = Sys.argv.(1)

let ocaml_file =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
  let str = parse_ocaml_structure_lb lexbuf in
  structure ~filename:fname str

open Gospel2ocaml

let translated_ocam_file =
  Conversion.s_structure ocaml_file

let fname_out =
  fname ^ ".out"

let () =
  let open Format in
  let open Upretty_printer in
  let cout = open_out fname_out in
  let fout = formatter_of_out_channel cout in
  fprintf fout "%a@." s_structure translated_ocam_file
