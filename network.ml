(* Module de Reseau de Kahn sur le reseau *)

open Printf;;
open Unix;;
open Marshal;;

exception No_Channel;;

let addr_l = ref [];;
let port_nb = 5555;;
let parent = ref None;; (* ADDR_INET(inet_addr_loopback, port_nb);; *)

let is_serveur = ref false;;

let get_my_addr () =
  (gethostbyname(gethostname())).h_addr_list.(0)
;;

let rec read_from_channel c_in =
  try
    from_channel c_in
  with
  |End_of_file -> read_from_channel c_in
;;

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
  type 'a in_port = string;;
  type 'a out_port = string;;

  let id_channel = ref 0;;
  let pipe = ref [];;
  
  let create_pipe () =
    let in_pipe, out_pipe = Unix.pipe () in
    (Unix.in_channel_of_descr in_pipe), (Unix.out_channel_of_descr out_pipe)
  ;;

  let new_channel () =
    let c = (gethostname ()) ^ (string_of_int !id_channel) in
    let (c_in, c_out) = create_pipe () in
    pipe := (c, (c_in, c_out))::(!pipe);
    id_channel := !id_channel + 1;
    (c, c)
  ;;

  let put v c () =
    try
      let (c_in, c_out) = List.assoc c !pipe in
      output_value c_out v
    with
    |Not_found -> begin
      match !parent with
      |None -> raise No_Channel
      |Some(parent) ->
        let (c_in, c_out) = open_connection parent in
        to_channel c_out "PUT" [];
        to_channel c_out c [];
        to_channel c_out v [];
        flush c_out
    end
  ;;

  let rec get c () =
    try
      let (c_in, c_out) = List.assoc c !pipe in
      input_value c_in
    with
    |Not_found -> begin
      match !parent with
      |None -> raise No_Channel
      |Some(parent) ->
        let (c_in, c_out) = open_connection parent in
        to_channel c_out "GET" [];
        to_channel c_out c [];
        flush c_out;
        let rec wait_answer () =
          let (mess : string) = input_value c_in in
          match mess with
          |"GET_ANSWER" -> input_value c_in
          |"GET_EMPTY" -> get c ()
          |_ -> wait_answer ()
        in
        wait_answer ()
    end
  ;;
  
  let doco_fork l () =
    let rec distrib l =
      match l with
      |[] -> 1
      |head::tail -> begin
        match Unix.fork () with
        |0 -> head (); exit 0
        |pid -> distrib tail
      end
    in
    let rec wait_all l =
      match l with
      |[] -> ()
      |head::tail ->
        Unix.wait ();
        wait_all tail
    in
    match distrib l with
    |1 -> wait_all l
    |_ -> ()
  ;;

  let rec doco_network l () =
    let rec distrib l addr_l buff_l out =
      match addr_l with
      |[] -> distrib l buff_l addr_l out
      |addr::addr_l -> begin
        match l with
        |[] -> out
        |h::t ->
          let (c_in, c_out) = open_connection addr in
          to_channel c_out "INIT" [];
          to_channel c_out h [Marshal.Closures];
          flush c_out;
          Queue.push (c_in, c_out) out;
          distrib t addr_l (addr::buff_l) out
      end
    in
    let rec wait_all chann_l =
      if Queue.is_empty chann_l then
        ()
      else begin
        let (c_in, c_out) = Queue.pop chann_l in
        let (mess : string) = input_value c_in in
        (match mess with
        |"TERMINE" ->
          input_value c_in;
          shutdown_connection c_in;
          close_in c_in;
          close_out c_out
        |"PUT" ->
          let c = input_value c_in in
          let v = input_value c_in in
          put v c ()
        |"GET" -> begin
          let c = input_value c_in in
          let v = get c () in
          to_channel c_out "GET_ANSWER" [];
          to_channel c_out v []
        end
        |_ -> Queue.push (c_in, c_out) chann_l);
        wait_all chann_l
      end
    in
    let chann_l = distrib l !addr_l [] (Queue.create ()) in
    wait_all chann_l
  ;;
  
  let doco l () =
    if !addr_l = [] then
      doco_fork l ()
    else
      doco_network l ()
  ;;

  let return v = (fun () -> v);;

  let bind e e' () =
    let v = e () in
    e' v ()
  ;;
  
  let run e =
    e ()
    (* match !parent with
    |Some addr -> e ()
    |None ->
      print_endline "Tentative de connection";
      let (c_in, c_out) = open_connection (ADDR_INET(get_my_addr (), port_nb)) in
      print_endline "Connection reussi !";
      to_channel c_out "INIT" [Closures];
      flush c_out;
      print_endline "Envoie de INIT !";
      to_channel c_out e [No_sharing ; Closures ; Compat_32];
      flush c_out;
      print_endline "Envoie du proc";

      print_endline "flush, depart des paquets";
      let rec wait_answer () =
        let (mess : string) = read_from_channel c_in in
        match mess with
        |"TERMINE" -> read_from_channel c_in
        |_ -> wait_answer ()
      in
      wait_answer () *)
  ;;
end

(*****)

(* Serveur d'execution *)

let make_addr_l file =
  let rec aux chann out =
    try
      let addr = ADDR_INET(inet_addr_of_string (input_line chann), port_nb) in
      aux chann (addr::out)
    with
    |End_of_file -> out
  in
  let chann = open_in file in
  let addr_l = aux chann [] in
  close_in chann;
  addr_l
;;

let server p =
  if !is_serveur then
    let addr = get_my_addr () in
    let rec exec_proc c_in c_out =
      print_endline "Connection etabli !";
      parent := Some(getsockname (descr_of_in_channel c_in));
      print_endline "parent initailise !";
      let (init : string) = read_from_channel c_in in
      print_endline "recuperation du message d initialisation !";
      if init <> "INIT" then
        exec_proc c_in c_out
      else begin
        printf "%s" init; print_endline "";
        let proc = read_from_channel c_in in
        print_endline "proc recupere !!!";
        let v = Kahn.run proc in
        output_value c_out "TERMINE";
        output_value c_out v
      end
    in
    establish_server exec_proc (ADDR_INET(addr, port_nb))
  else
    Kahn.run p
;;

(*****)

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
      (K.get qi) >>= (fun v -> Format.printf "l%d@." v; print_endline ""; loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Kahn)

(*****)

let input_file = ref "";;

let speclist = [("-s", Arg.Set is_serveur, "Enables serveur mode")];;

let usage = "usage: serveur addr.txt [-s]";;

let set_file f s = f := s;;

let _ =
  Arg.parse speclist (set_file input_file) usage;
  
  if !input_file <> "" then
    addr_l := make_addr_l !input_file;

  List.iter (fun x -> let ADDR_INET(a, p) = x in print_endline (string_of_inet_addr a)) !addr_l;
  
  server E.main
;;










































