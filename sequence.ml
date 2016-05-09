(* Module de Reseaux de Kahn sequentiel *)
(*
  Partie pris : un processus laisse la main
  seulement lorsque qu'il effectue une ecriture
  ou bien lorsqu'il effectue une lecture d'une 
  chaine vide
*)

open Kahn;;


module Kahn: S = struct
  type 'a process = ('a -> unit) -> unit;;
  type 'a channel = 'a Queue.t;;
  type 'a in_port = 'a channel;;
  type 'a out_port = 'a channel;;

  exception Empty_channel of (unit -> unit);;
  exception Channel_changed of (unit -> unit);;
  exception ERROR;;

  let ch = ref 0;;
  let empt = ref 0;;

  let new_channel () =
    let q = Queue.create () in
    (q, q)
  ;;

  let bind e e' next =
    e (fun v -> e' v next)
  ;;

  let run e =
    let out = ref None in
    e (fun x -> out := Some x; ());
    match !out with
    |None -> raise ERROR
    |Some x -> x
  ;;

  let return v next =
    next v
  ;;

  let put v c next =
    Queue.push v c;
    raise (Channel_changed next)
  ;;
(* ocamlrun = backtrace *)
  let rec get c next =
    try
      let v = Queue.pop c in
      next v
    with
    |Queue.Empty ->
       raise (Empty_channel (fun () -> get c next))
  ;;

  let doco l next =
    let proc_ord = Queue.create () in
    List.iter (fun x -> Queue.push (fun () -> run x) proc_ord) l;
    while not(Queue.is_empty proc_ord) do
      let proc = Queue.pop proc_ord in
      try
        proc ()
      with    
      |Empty_channel(new_proc) ->
        empt := !empt + 1;
        Queue.push new_proc proc_ord
      |Channel_changed(continuation) ->
        ch := !ch +1;
        Queue.push  continuation proc_ord
    done;
    next ()
  ;;

  let return v next =
    next v
  ;;
end
