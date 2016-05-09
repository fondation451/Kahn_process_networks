(* Module de Reseau de Kahn sur le reseau *)

open Kahn;;
open Unix;;

exception No_Channel;;

let addr_l = ref [];;
let port_nb = 5555;;
let parent = ref None;; (* ADDR_INET(inet_addr_loopback, port_nb);; *)

let get_my_addr () =
  (gethostbyname(gethostname())).h_addr_list.(0)
;;

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
        output_value c_out "PUT";
        output_value c_out c;
        output_value c_out v;
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
        output_value c_out "GET";
        output_value c_out c;
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
          output_value c_out "INIT";
          output_value c_out h;
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
          output_value c_out "GET_ANSWER";
          output_value c_out v
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
    match !parent with
    |Some addr -> e ()
    |None ->
      let (c_in, c_out) = open_connection (ADDR_INET(inet_addr_loopback, port_nb)) in
      output_value c_out "INIT";
      output_value c_out e;
      flush c_out;
      let rec wait_answer () =
        let (mess : string) = input_value c_in in
        match mess with
        |"TERMINE" -> input_value c_in
        |_ -> wait_answer ()
      in
      wait_answer ()
  ;;
end









































