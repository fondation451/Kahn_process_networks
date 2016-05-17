(* Module de Reseau de Kahn sur le reseau *)

open Printf;;
open Unix;;
open Marshal;;

exception No_Channel;;
exception BREAK;;

let get_my_addr () =
  printf "ESSAIE gethostbyname"; print_endline "";
  let tmp = (gethostbyname(gethostname())).h_addr_list.(0) in
  printf "gethostbyname REUSSI"; print_endline "";
  tmp
;;

let create_pipe () =
  let in_pipe, out_pipe = Unix.pipe () in
  (Unix.in_channel_of_descr in_pipe), (Unix.out_channel_of_descr out_pipe)
;;

(* let (network_in, network_out) = create_pipe ();; *)
let net_pipe = ref (Some(create_pipe ()));;

let addr_l = ref [];;
let port_nb = 2529;;
let local_addr = ref (ADDR_INET(get_my_addr (), port_nb));;

let id_channel = ref 0;;
let pipe = ref [];;

let bus = ref (Some(create_pipe ()));;
let id = ref 0;;
(* let id = ref ((gethostname ()) ^ (string_of_int !id_compt));; *)

let nb_proc = ref 0;;

let is_serveur = ref false;;


let addr_to_string addr =
  let ADDR_INET(a, p) = addr in
  string_of_inet_addr a
;;

let same_computer c_in =
  (addr_to_string (getsockname (descr_of_in_channel c_in))) = (string_of_inet_addr (get_my_addr ()))
;;

let close_connection c_in c_out =
  shutdown_connection c_in;
  (* close_in c_in;
  close_out c_out *)
;;

let rec read_from_channel c_in =
  try
    (* printf "ESSAIE (processus %d)" (getpid ()); print_endline ""; *)
    let tmp = from_channel c_in in
    tmp
  with
  |End_of_file -> read_from_channel c_in
;;

let rec read_from_bus pid bus_in bus_out =
  try
    (* printf "Lecture bus %d (processus %d)" pid (getpid ()); print_endline ""; *)
    let (id, v) = input_value bus_in in
    if id = pid then begin
      printf "TROUVER"; print_endline "";
      v
    end
    else
      begin (* printf "Mauvaise lecture pid = %d != %d" pid id; print_endline ""; *) output_value bus_out (id, v); flush bus_out; sleep 1; read_from_bus pid bus_in bus_out end
  with
  |End_of_file -> read_from_bus pid bus_in bus_out
;;

let rec output_funcion c_out f =
  let pipe_save = !pipe in
  let net_pipe_save = !net_pipe in
  let bus_save = !bus in
  bus := None;
  net_pipe := None;
  pipe := [];
  to_channel c_out f [Marshal.Closures];
  pipe := pipe_save;
  net_pipe := net_pipe_save;
  bus := bus_save
;;

let rec wait_answer code c_in =
  let (answer : string) = read_from_channel c_in in
  if answer = code then
    read_from_channel c_in
  else
    wait_answer code c_in
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
    printf "new_channel %d !!!" (getpid ()); print_endline "";
    let (c_in, c_out) = open_connection !local_addr in
    to_channel c_out "NEW_CHANNEL" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    flush c_out;
    close_connection c_in c_out;
    raise Exit
  ;;

  let put v c () =
    printf "put %d !!!" (getpid ()); print_endline "";
    let (id_c, addr_c) = c in
    let (c_in, c_out) = open_connection addr_c in
    to_channel c_out "PUT" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    to_channel c_out id_c [];
    to_channel c_out v [];
    flush c_out;
    close_connection c_in c_out;
    ()
  ;;

  let rec get c () =
    printf "get %d !!!" (getpid ()); print_endline "";
    let (id_c, addr_c) = c in
    let (c_in, c_out) = open_connection addr_c in
    to_channel c_out "GET" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    to_channel c_out id_c [];
    flush c_out;
    close_connection c_in c_out;
    raise Exit
  ;;
  
  let doco l () =
    printf "doco %d !!!" (getpid ()); print_endline "";
    let rec aux l c_out =
      match l with
      |[] -> ()
      |h::t -> output_funcion c_out h; aux t c_out
    in
    let (c_in, c_out) = open_connection !local_addr in
    to_channel c_out "DOCO" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    to_channel c_out (List.length l) [];
    aux l c_out;
    flush c_out;
    close_connection c_in c_out;
    raise Exit
  ;;

  let return v () = v;;

  let bind e e' () =
    printf "bind %d !!!" (getpid ()); print_endline "";
    let (c_in, c_out) = open_connection !local_addr in
    to_channel c_out "BIND" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    output_funcion c_out e;
    output_funcion c_out e';
    flush c_out;
    close_connection c_in c_out;
    raise Exit
  ;;
  
  let run e = try e () with Exit -> raise Exit;;
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
  List.rev addr_l
;;

let fork_request addr id_out from_addr proc =
  let (c_in, c_out) = open_connection addr in
  to_channel c_out "FORK" [];
  to_channel c_out id_out [];
  to_channel c_out from_addr [];
  output_funcion c_out proc;
  flush c_out;
  close_connection c_in c_out
;;

let termine_request addr v id_out =
  let (c_in, c_out) = open_connection addr in
  to_channel c_out "TERMINE" [];
  to_channel c_out v [];
  to_channel c_out id_out [];
  flush c_out;
  close_connection c_in c_out
;;

let read_request_header c_in =
  let id_out = read_from_channel c_in in
  let from_addr = read_from_channel c_in in
  id_out, from_addr
;;

let new_id () =
  let out = !id in
  id := !id + 1; out
;;

let rec request_manager () =
  let Some(network_in, network_out) = !net_pipe in
  let Some(bus_in, bus_out) = !bus in
  printf "                                                                                   boulce"; print_endline "";
  let (request : string) = read_from_channel network_in in
  printf "                                                                                   boulce"; print_endline "";
  match request with
  |"FORK" -> begin
    let id_out, from_addr = read_request_header network_in in
    let proc = read_from_channel network_in in
    printf "REQUETE FORK : id_out = %d | from_addr = %s" id_out (addr_to_string from_addr); print_endline "";
    match Unix.fork () with
    |0 ->
      let v = try Kahn.run proc with Exit -> read_from_bus (getpid ()) bus_in bus_out in
      termine_request from_addr v id_out
    |pid -> request_manager ()
  end
  |"BIND" -> begin
    let id_out, from_addr = read_request_header network_in in
    let fst_proc = read_from_channel network_in in
    let snd_proc = read_from_channel network_in in
    printf "REQUETE BIND : id_out = %d" id_out; print_endline "";
    match Unix.fork () with
    |0 ->
      let fst_v = try Kahn.run fst_proc with Exit -> read_from_bus (getpid ()) bus_in bus_out in
      let snd_v = try Kahn.run (snd_proc fst_v) with Exit -> read_from_bus (getpid ()) bus_in bus_out in
      termine_request from_addr snd_v id_out;
    |pid -> request_manager ()
  end
  |"NEW_CHANNEL" -> begin
    let id_out, from_addr = read_request_header network_in in
    let c = (gethostname ()) ^ (string_of_int !id_channel) in
    printf "REQUETE NEW_CHANNEL : proc_id = %d | nb_c = %d | channel = %s" id_out !id_channel c; print_endline "";
    let (pipe_in, pipe_out) = create_pipe () in
    pipe := (c, (pipe_in, pipe_out))::(!pipe);
    id_channel := !id_channel + 1;
    termine_request from_addr ((c, !local_addr), (c, !local_addr)) id_out;
    request_manager ()
  end
  |"PUT" -> begin
    let id_out, from_addr = read_request_header network_in in
    let id_c = read_from_channel network_in in
    let v = read_from_channel network_in in
    printf "REQUETE PUT : channel = %s| id_out = %d | from_addr = %s" id_c id_out (addr_to_string from_addr); print_endline "";
    printf "Recherche de %s" id_c; print_endline "";
    let (pipe_in, pipe_out) = List.assoc id_c !pipe in
    printf "%s trouve !" id_c; print_endline "";
    output_value pipe_out v;
    flush network_out;
    termine_request from_addr () id_out;
    request_manager ()
  end
  |"GET" -> begin
    let id_out, from_addr = read_request_header network_in in
    let id_c = read_from_channel network_in in
    printf "REQUETE GET : channel = %s | id_out = %d | from_addr = %s" id_c id_out (addr_to_string from_addr); print_endline "";
    match Unix.fork () with
    |0 ->
      printf "Recherche de %s" id_c; print_endline "";
      let (pipe_in, pipe_out) = List.assoc id_c !pipe in
      printf "%s trouve !" id_c; print_endline "";
      let v = read_from_channel pipe_in in
      termine_request from_addr v id_out
    |pid -> request_manager ()
  end
  |"DOCO" -> begin
    let id_out, from_addr = read_request_header network_in in
    let nb_proc = read_from_channel network_in in
    let l_proc = let rec aux x = if x = 0 then [] else (read_from_channel network_in)::(aux (x-1)) in aux nb_proc in
    let rec distrib_and_wait l addr_l buff_l l_pid =
      match addr_l with
      |[] -> distrib_and_wait l buff_l addr_l l_pid
      |addr::tail_addr -> begin
        match l with
        |[] -> begin
          match Unix.fork () with
          |0 ->
            print_endline "doco ca degage pas encore !";
            List.iter (fun pid -> read_from_bus pid bus_in bus_out) l_pid;
            print_endline "doco ca degage !";
            termine_request from_addr () id_out
          |pid -> request_manager ()
        end
        |h::t -> begin
          match Unix.fork () with
          |0 ->
            let (c_in, c_out) = open_connection addr in
            to_channel c_out "FORK" [];
            to_channel c_out (getpid ()) [];
            to_channel c_out !local_addr [];
            output_funcion c_out h;
            flush c_out;
            (* close_connection c_in c_out; *)
            exit 0
          |pid -> distrib_and_wait t tail_addr (addr::buff_l) (pid::l_pid)
        end
      end
    in
    distrib_and_wait l_proc !addr_l [] []
  end
  |"TERMINE" -> begin
    let v = read_from_channel network_in in
    let id_out = read_from_channel network_in in
    printf "REQUETE TERMINE : id_out = %d" id_out; print_endline "";
    output_value bus_out (id_out, v);
    flush bus_out;
    request_manager ()
  end
  |"REMOVE_PIPE" -> begin
    let pipe_id = read_from_channel network_in in
    printf "REQUETE REMOVE_PIPE : pipe_id = %d" pipe_id; print_endline "";
    request_manager ()
  end
  |_ -> request_manager ()
;;

let network_buffer () =
  let Some(network_in, network_out) = !net_pipe in
  let rec aux c_in c_out =
    printf "NETWORK BUFFER :::: Connexcion entrante %s !!!" (addr_to_string (getsockname (descr_of_in_channel c_in))); print_endline "";
    let (request : string) = read_from_channel c_in in
    printf "BUFFERISATION de %s" request; print_endline "";
    match request with
    |"FORK" ->      
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let proc = read_from_channel c_in in
      output_value network_out "FORK";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_funcion network_out proc;
      flush network_out
    |"BIND" ->
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let fst_proc = read_from_channel c_in in
      let snd_proc = read_from_channel c_in in
      output_value network_out "BIND";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_funcion network_out fst_proc;
      output_funcion network_out snd_proc;
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
      output_value network_out "GET";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out c;
      flush network_out
    |"DOCO" ->
      let id_out = read_from_channel c_in in
      let from_addr = read_from_channel c_in in
      let nb_proc = read_from_channel c_in in
      let l_proc = let rec aux x = if x = 0 then [] else (read_from_channel c_in)::(aux (x-1)) in aux nb_proc in
      output_value network_out "DOCO";
      output_value network_out id_out;
      output_value network_out from_addr;
      output_value network_out nb_proc;
      List.iter (fun proc -> output_funcion network_out proc) l_proc;
      flush network_out
    |"TERMINE" ->
      let v = read_from_channel c_in in
      let id_out = read_from_channel c_in in
      output_value network_out "TERMINE";
      output_value network_out v;
      output_value network_out id_out;
      flush network_out
    |"REMOVE_PIPE" ->
      let pipe_id = read_from_channel c_in in
      output_value network_out "REMOVE_PIPE";
      output_value network_out pipe_id;
  in
  establish_server aux !local_addr
;;

let serveur () =
  match Unix.fork () with
  |0 -> network_buffer (); exit 0
  |pid -> request_manager (); printf "(^_^)"; print_endline ""
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
  
  if !input_file <> "" then begin
    let tmp = make_addr_l !input_file in
    local_addr := List.hd tmp;
    addr_l := List.tl tmp
  end
  else
    print_endline "Fichier d'adresse manquant !";

  List.iter (fun x -> let ADDR_INET(a, p) = x in print_endline (string_of_inet_addr a)) !addr_l;
  
  if !is_serveur then
    serveur ()
  else begin
    let (c_in, c_out) = open_connection !local_addr in
    to_channel c_out "FORK" [];
    to_channel c_out (getpid ()) [];
    to_channel c_out !local_addr [];
    output_funcion c_out E.main;
    flush c_out;
    close_connection c_in c_out
  end
;;










































