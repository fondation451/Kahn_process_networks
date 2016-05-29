(* Module de Reseaux de Kahn sequentiel *)
(*
  Une action par processus
*)

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

module RSA (K : S) = struct
  module K = K;;
  module Lib = Lib(K);;
  open Lib;;
  
  let is_prime n =
    let rec aux tmp fin =
      if tmp > fin then
        true
      else
        n mod tmp <> 0 && aux (tmp+1) fin
    in
    aux 2 (int_of_float (sqrt (float_of_int n)))
  ;;
  
  let find_big_prime () =
    print_endline "OK";
    let rec find_ a = if is_prime a then a else find_ (a+1) in
    print_endline "BON";
    let a = Random.int 1000 in
    let b = Random.int 1000 in
    find_ a, find_ b
  ;;
  
  let rec pgcd a b =
    if b = 0 then
      a
    else
      pgcd (a/b) (a mod b)
  ;;
  
  let inverse_mod e phi =
    let rec aux r u v r' u' v' =
      if r' = 0 then
        u
      else
        let q = r / r' in
        aux r' u' v' (r - q * r') (u - q * u') (v - q * v')
    in
    aux e 1 0 phi 0 1
  ;;
  
  let gen_e phi =
    let rec aux out = if pgcd phi out = 1 then out else aux (out+1) in
    aux (Random.int phi)
  ;;
  
  let gen_key () =
    print_endline "rtyui1";
    let p, q = find_big_prime () in
    print_endline "rtyui2";
    let n = p * q in
    print_endline "rtyui3";
    let phi = (p-1) * (q-1) in
    print_endline "rtyui4";
    let e = gen_e phi in
    print_endline "rtyui5";
    let d = inverse_mod e phi in
    print_endline "rtyui";
    ((n, e), (n, d))
  ;;
  
  let power_mod x p n =
    let rec aux p out =
      if p = 0 then
        out
      else
        aux (p-1) ((out * x) mod n)
    in
    aux p 1
  ;;
  
  let crypt_fun m c =
    let n, e = c in
    power_mod m e n
  ;;
  
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "||||||||||||||||||||||||||||||||||||||||||||||||||||||||%d@." v; print_endline ""; loop ())
    in
    loop ()
  ;;
  
  let crypt t c : int K.process =
    K.return (crypt_fun t c)
  ;;
  
  let doco_crypt c (qi : int K.in_port) (qo : int K.out_port) : unit K.process =
    let texte = "" in
    let fin = String.length texte in
    let rec create_proc n out =
      if n >= fin then
        out
      else
        create_proc (n+1) (((crypt (Char.code texte.[n]) c) >>= (fun v -> K.put v qo))::(output qi)::out)
    in
    K.doco (create_proc 0 [])
  ;;

  let main : unit K.process =
    Random.init 30;
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) ->
      let cc, cd = gen_key () in
      print_endline "YOUPI";
      doco_crypt cc q_in q_out)
  ;;

end

module E = RSA(Kahn)

let _ =
  Kahn.run E.main
;;










































