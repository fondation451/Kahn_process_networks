(* Module de Reseau de Kahn sur le reseau *)

open Printf;;
open Unix;;
open Marshal;;

exception No_Channel;;
exception BREAK;;

let create_pipe () =
  let in_pipe, out_pipe = Unix.pipe () in
  (Unix.in_channel_of_descr in_pipe), (Unix.out_channel_of_descr out_pipe)
;;

let net_pipe = ref [create_pipe ()];;
let port_nb = 2672;;
let addr_file = "addr.txt";;
let id_channel = ref 0;;
let pipe = ref [];;
let is_serveur = ref false;;

let get_my_addr () =
  ADDR_INET((Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0), port_nb)
;;

let make_addr_l file =
  let rec aux chann out =
    try
      let addr = ADDR_INET(inet_addr_of_string (input_line chann), port_nb) in
      aux chann (addr::out)
    with
    |End_of_file -> out
  in
  try
    let chann = open_in file in
    let addr_l = aux chann [(get_my_addr ())] in
    close_in chann;
    List.rev addr_l
  with Sys_error(_) -> [(get_my_addr ())]
;;

let addr_to_string addr =
  let Unix.ADDR_INET(a, p) = addr in
  Unix.string_of_inet_addr a
;;

let close_connection c_in =
  shutdown_connection c_in;
  close_in c_in
;;

let rec read_from_channel c_in =
  try
    (* printf "ESSAIE (processus %d)" (getpid ()); print_endline ""; *)
    let tmp = input_value c_in in
    tmp
  with
  |End_of_file -> read_from_channel c_in
;;

let read_from_bus pid =
  let chann = open_in ("FIFO/" ^ (gethostname ()) ^ (string_of_int pid)) in
  let rec aux () =
    try
      input_value chann
    with End_of_file -> aux ()
  in
  let out = aux () in
  close_in chann;
  out
;;

let rec write_to_bus pid v =
  let chann = open_out ("FIFO/" ^ (gethostname ()) ^ (string_of_int pid)) in
  output_value chann v;
  close_out chann
;;

let rec output_funcion c_out f =
  let pipe_save = !pipe in
  let net_pipe_save = !net_pipe in
  net_pipe := [];
  pipe := [];
  to_channel c_out f [Marshal.Closures];
  pipe := pipe_save;
  net_pipe := net_pipe_save;
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
  type 'a in_port = string * sockaddr;;
  type 'a out_port = string * sockaddr;;

  let new_channel () =
    (* printf "new_channel %d !!!" (getpid ()); print_endline ""; *)
    let (c_in, c_out) = open_connection (get_my_addr ()) in
    output_value c_out "NEW_CHANNEL";
    output_value c_out (getpid ());
    output_value c_out (get_my_addr ());
    flush c_out;
    close_connection c_in;
    raise Exit
  ;;

  let put v c () =
    (* printf "put %d !!!" (getpid ()); print_endline ""; *)
    let (id_c, addr_c) = c in
    let (c_in, c_out) = open_connection addr_c in
    output_value c_out "PUT";
    output_value c_out (getpid ());
    output_value c_out (get_my_addr ());
    output_value c_out id_c;
    output_value c_out v;
    flush c_out;
    close_connection c_in;
    raise Exit
  ;;

  let rec get c () =
    (* printf "get %d !!!" (getpid ()); print_endline ""; *)
    let (id_c, addr_c) = c in
    let (c_in, c_out) = open_connection addr_c in
    output_value c_out "GET";
    output_value c_out (getpid ());
    output_value c_out (get_my_addr ());
    output_value c_out id_c;
    flush c_out;
    close_connection c_in;
    raise Exit
  ;;
  
  let doco l () =
    let rec distrib_and_wait l addr_l buff_l out =
      match addr_l with
      |[] -> distrib_and_wait l buff_l addr_l out
      |addr::tail_addr -> begin
        match l with
        |[] -> out
        |h::t ->
          let id = getpid () in
          let (c_in, c_out) = open_connection addr in
          output_value c_out "FORK";
          output_value c_out id;
          output_value c_out (get_my_addr ());
          output_funcion c_out h;
          flush c_out;
          close_connection c_in;
          distrib_and_wait t tail_addr (addr::buff_l) (1::out)
      end
    in
    let l_addr = make_addr_l addr_file in
    let wait_list = distrib_and_wait l l_addr [] [] in
    List.iter (fun x -> read_from_bus (getpid ())) wait_list
  ;;

  let return v () = v;;

  let bind e e' () =
    let v = try e () with Exit -> read_from_bus (getpid ()) in
    let out = try e' v () with Exit -> read_from_bus (getpid ()) in
    out
  ;;
  
  let run e = try e () with Exit -> raise Exit;;
end

(*****)

(* Serveur d'execution *)

let termine_request network_out addr v id_out =
  if addr = (get_my_addr ()) then begin
    output_value network_out "TERMINE";
    output_value network_out v;
    output_value network_out id_out;
    flush network_out;
  end
  else begin
    let (c_in, c_out) = open_connection addr in
    output_value c_out "TERMINE";
    output_value c_out v;
    output_value c_out id_out;
    flush c_out;
    close_connection c_in
  end
;;

let read_request_header c_in =
  let id_out = read_from_channel c_in in
  let from_addr = read_from_channel c_in in
  id_out, from_addr
;;

let rec request_manager () =
  let (network_in, network_out) = List.hd !net_pipe in
  (* printf "                                                                                   boulce"; print_endline ""; *)
  let (request : string) = read_from_channel network_in in
  (* printf "                                                                                   boulce"; print_endline ""; *)
  match request with
  |"FORK" -> begin
    let id_out, from_addr = read_request_header network_in in
    let proc = read_from_channel network_in in
    (* printf "REQUETE FORK : id_out = %d | from_addr = %s" id_out (addr_to_string from_addr); print_endline ""; *)
    match Unix.fork () with
    |0 -> begin
      match Unix.fork () with
      |0 ->
        (* printf "JUSTE AVANT"; print_endline ""; *)
        (* dup2 (descr_of_out_channel network_out) stdout; *)
        mkfifo ("FIFO/" ^ (gethostname ()) ^ (string_of_int (getpid ()))) 0o640;
        let v = try Kahn.run proc with Exit -> read_from_bus (getpid ()) in
        termine_request network_out from_addr v id_out;
        exit 0
      |pid -> exit 0
    end
    |pid -> wait (); request_manager ()
  end
  |"NEW_CHANNEL" -> begin
    let id_out, from_addr = read_request_header network_in in
    let c = (gethostname ()) ^ (string_of_int !id_channel) in
    (* printf "REQUETE NEW_CHANNEL : proc_id = %d | nb_c = %d | channel = %s" id_out !id_channel c; print_endline ""; *)
    let (pipe_in, pipe_out) = create_pipe () in
    pipe := (c, (ref 0, (pipe_in, pipe_out)))::(!pipe);
    id_channel := !id_channel + 1;
    termine_request network_out from_addr ((c, (get_my_addr ())), (c, (get_my_addr ()))) id_out;
    request_manager ()
  end
  |"PUT" -> begin
    let id_out, from_addr = read_request_header network_in in
    let id_c = read_from_channel network_in in
    let v = read_from_channel network_in in
    (* printf "REQUETE PUT : channel = %s| id_out = %d | from_addr = %s" id_c id_out (addr_to_string from_addr); print_endline "";
    printf "Recherche de %s" id_c; print_endline ""; *)
    let (compt, (pipe_in, pipe_out)) = List.assoc id_c !pipe in
    (* printf "%s trouve, taille %d !" id_c !compt; print_endline ""; *)
    if !compt < 2000 then begin
      output_value pipe_out v;
      (* printf "bon"; print_endline ""; *)
      flush pipe_out;
      compt := !compt + 1;
      termine_request network_out from_addr () id_out
    end
    else begin
      output_value network_out "PUT";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out id_c;
      output_value network_out v;
      flush network_out;
    end;
    (* printf "c'est pas cool"; print_endline "";
    printf "presque"; print_endline "";
    printf "normale je suis lz"; print_endline ""; *)
    request_manager ()
  end
  |"GET" -> begin
    let id_out, from_addr = read_request_header network_in in
    let id_c = read_from_channel network_in in
    (* printf "REQUETE GET : channel = %s | id_out = %d | from_addr = %s" id_c id_out (addr_to_string from_addr); print_endline "";
    printf "Recherche de %s" id_c; print_endline ""; *)
    let (compt, (pipe_in, pipe_out)) = List.assoc id_c !pipe in
    (* printf "%s trouve, taille %d !" id_c !compt; print_endline ""; *)
    if !compt > 0 then begin
      let v = read_from_channel pipe_in in
      compt := !compt - 1;
      termine_request network_out from_addr v id_out
    end
    else begin
      output_value network_out "GET";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out id_c;
    end;
    request_manager ()
  end
  |"TERMINE" -> begin
    let v = read_from_channel network_in in
    let id_out = read_from_channel network_in in
    (* printf "REQUETE TERMINE : id_out = %d" id_out; print_endline ""; *)
    write_to_bus id_out v;
    request_manager ()
  end
  |_ -> request_manager ()
;;

let network_buffer () =
  let (network_in, network_out) = List.hd !net_pipe in
  let rec aux c_in c_out =
    (* printf "NETWORK BUFFER :::: Connexcion entrante %s !!!" (addr_to_string (getsockname (descr_of_in_channel c_in))); print_endline ""; *)
    let (request : string) = read_from_channel c_in in
    (* printf "BUFFERISATION de %s" request; print_endline ""; *)
    (match request with
    |"FORK" ->      
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let proc = read_from_channel c_in in
      output_value network_out "FORK";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_funcion network_out proc;
      flush network_out
    |"NEW_CHANNEL" ->
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      output_value network_out "NEW_CHANNEL";
      output_value network_out id_out;
      output_value network_out from_addr;
      flush network_out
    |"PUT" ->
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let c = read_from_channel c_in in
      let v = read_from_channel c_in in
      output_value network_out "PUT";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out c;
      output_value network_out v;
      flush network_out
    |"GET" ->
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let c = read_from_channel c_in in
      (* printf "Lecture faite"; print_endline ""; *)
      output_value network_out "GET";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out c;
      flush network_out;
      (* printf "Envoi fait"; print_endline ""; *)
    |"TERMINE" ->
      let v = read_from_channel c_in in
      let id_out = read_from_channel c_in in
      output_value network_out "TERMINE";
      output_value network_out v;
      output_value network_out id_out;
      flush network_out);
    close_in c_in
  in
  establish_server aux (get_my_addr ())
;;

let serveur () =
  system "mkdir FIFO";
  system "rm FIFO/*";
  match Unix.fork () with
  |0 -> network_buffer (); exit 0
  |pid -> request_manager ()
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
      (K.get qi) >>= (fun v -> Format.printf "||||||||||||||||||||||||||||||||||||||||||||||||||||||||%d@." v; print_endline ""; loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Kahn)

(*****)


let _ =
  is_serveur := Sys.argv.(1) = "1";
  
  printf "CA DEGAGE %d %s !!!" (getpid ()) (addr_to_string (get_my_addr ())); print_endline "";
  
  if !is_serveur then
    serveur ()
  else begin
    let (c_in, c_out) = open_connection (get_my_addr ()) in
    to_channel c_out "FORK" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out (get_my_addr ()) [];
    output_funcion c_out E.main;
    flush c_out;
    close_connection c_in
  end
;;






































