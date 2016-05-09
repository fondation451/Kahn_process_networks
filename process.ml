(* Module de Reseau de Kahn avec processus lourd *)

open Kahn;;
open Unix;;


module Kahn: S = struct
  type 'a process = (unit -> 'a);;
  type 'a in_port = in_channel;;
  type 'a out_port = out_channel;;

  let new_channel () =
    let in_pipe, out_pipe = Unix.pipe () in
    (Unix.in_channel_of_descr in_pipe), (Unix.out_channel_of_descr out_pipe)
  ;;

  let put v c () =
    output_value c 1995;
    output_value c v
  ;;

  let rec get c () =
    let (test : int) = input_value c in
    match test with
    |1995 -> input_value c
    |_ -> get c ()
  ;;

  let doco l () =
    let rec fork_all l =
      match l with
      |[] -> 1
      |head::tail -> begin
        match Unix.fork () with
        |0 -> head (); exit 0
        |pid -> fork_all tail
      end
    in
    let rec wait_all l =
      match l with
      |[] -> ()
      |head::tail ->
        Unix.wait ();
        wait_all tail
    in
    match fork_all l with
    |1 -> wait_all l
    |_ -> ()
  ;;

  let return v = (fun () -> v);;

  let bind e e' () =
    let v = e () in
    e' v ()
  ;;

  let run e = e ();;
end
