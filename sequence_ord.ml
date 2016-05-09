(* Module de Reseaux de Kahn sequentiel *)
(*
  Une action par processus
*)

open Kahn;;
open Printf;;


module Kahn: S = struct
  type 'a process = ('a -> unit) -> unit;;
  type 'a channel = 'a Queue.t;;
  type 'a in_port = 'a channel;;
  type 'a out_port = 'a channel;;

  exception Stop of (unit -> unit);;
  exception ERROR;;

  let ch = ref 0;;
  let empt = ref 0;;
  
  let in_doco = ref [];;

  let new_channel () =
    let q = Queue.create () in
    (q, q)
  ;;
  
  let run e =
    let out = ref None in
    e (fun x -> out := Some x; ());
    match !out with
    |None -> raise ERROR
    |Some x -> x
  ;;

  let bind e e' next =
    let v = run e in
    if !in_doco <> [] then
      raise (Stop(fun () -> e' v next))
    else
      e' v next
  ;;

  let return v next =
    next v
  ;;

  let put v c next =
    Queue.push v c;
    next ()
  ;;
  
(* ocamlrun = backtrace *)
  
  let rec get c next =
    try
      let v = Queue.pop c in
      next v
    with
    |Queue.Empty ->
       raise (Stop (fun () -> get c next))
  ;;

  let doco l next =
    let proc_ord = Queue.create () in
    List.iter (fun x -> Queue.push (fun () -> run x) proc_ord) l;
    in_doco := true::(!in_doco);
    while not(Queue.is_empty proc_ord) do
      let proc = Queue.pop proc_ord in
      try
        proc ()
      with
      |Stop(new_proc) ->
        Queue.push new_proc proc_ord
    done;
    in_doco := List.tl (!in_doco);
    next ()
  ;;

  let return v next =
    next v
  ;;
end
