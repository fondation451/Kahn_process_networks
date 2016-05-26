(* Module de Reseau de Kahn avec processus lourd *)

open Unix;;
open Printf;;

(* Reseau de Kahn *)

module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end

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

(* Processus Test *)

module Example (K : S) = struct
  module K = K
  module Lib = Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "||||||||||||||||||||||||||||||||||||||||||||||||||||||||%d@." v; print_endline ""; loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Kahn)

let _ =
  Kahn.run E.main
;;




































