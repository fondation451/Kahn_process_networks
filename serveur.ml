(* Serveur pour le reseau de Kahn distribue *)

open Printf;;
open Unix;;
open Network;;

let input_file = ref "";;

let usage = "usage: serveur addr.txt";;

let set_file f s = f := s;;

let make_addr_l file =
  let rec aux chann out =
    try
      let addr = ADDR_INET(inet_addr_of_string (input_line chann), Network.port_nb) in
      aux chann (addr::out)
    with
    |End_of_file -> out
  in
  let chann = open_in file in
  let addr_l = aux chann [] in
  close_in chann;
  addr_l
;;

let server () =
  let addr = Network.get_my_addr () in
  let rec exec_proc c_in c_out =
    Network.parent := Some(getsockname (descr_of_in_channel c_in));
    let (init : string) = input_value c_in in
    if init <> "INIT" then
      exec_proc c_in c_out
    else begin
      let (proc : unit Kahn.process) = input_value c_in in
      let v = Network.Kahn.run proc in
      output_value c_out "TERMINE";
      output_value c_out v
    end
  in
  establish_server exec_proc (ADDR_INET(addr, Network.port_nb))
;;


let _ =
  Arg.parse [] (set_file input_file) usage;
  
  if !input_file <> "" then
    Network.addr_l := make_addr_l file;
  
  server ()
;;

























