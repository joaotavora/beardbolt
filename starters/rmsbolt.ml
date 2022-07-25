

(* OCaml beardbolt starter *)

(*
Local Variables:
beardbolt-command: "ocamlopt"
beardbolt-disassemble: nil
End:
*)

let rec fib num =
  if num <= 1 then
	num
  else
	fib (num - 1) + fib (num - 2);;

let print_fib num =
  Printf.printf "Fibonacci result of %d is %d.\n" num (fib 20);;

print_fib 20
