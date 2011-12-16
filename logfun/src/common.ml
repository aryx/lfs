(** Common utility functions. *)

(* space *)

let heap_size () : float = float_of_int (Gc.stat ()).Gc.heap_words *. float_of_int (Sys.word_size / 8)  (* in bytes *)

(* extensions a Weak *)

let weak_get_index : 'a Weak.t ref -> int =
  fun w ->
    let l = Weak.length !w in
    let i = ref 0 in
    while !i < l & Weak.check !w !i do incr i done;
    if !i >= l then begin
      let ar = Weak.create (l + 10)
      in Weak.blit !w 0 ar 0 l; w := ar end;
    !i

let weak_add : 'a Weak.t ref -> 'a -> unit =
  fun w x ->
    let i = weak_get_index w in
    Weak.set !w i (Some x)

let weak_iter : 'a Weak.t -> ('a -> unit) -> unit =
  fun w f ->
    for i=0 to Weak.length w - 1 do
      match Weak.get w i with
	None -> ()
      | Some x -> f x
    done

let list_of_weak : 'a Weak.t -> 'a list =
  fun w ->
    let res = ref [] in
    for i=0 to Weak.length w - 1 do
      match Weak.get w i with
	None -> ()
      |	Some x -> res := x::!res
    done;
    !res

(* List functionals *)
(* ---------------- *)

let iter_option : ('a -> unit) -> 'a option -> unit =
  fun f -> function
    | None -> ()
    | Some x -> f x

let fold_option : ('a -> 'b) -> 'b -> 'a option -> 'b =
  fun f e -> function
    | None -> e
    | Some x -> f x

let map_option : ('a -> 'b) -> 'a option -> 'b option =
  fun f -> function
    | None -> None
    | Some x -> Some (f x)

let rec filter : 'a option list -> 'a list =
  function
    | [] -> []
    | None::l -> filter l
    | Some x::l -> x::filter l

let rec mapfilter : ('a -> 'b option) -> 'a list -> 'b list =
  fun f -> function
      [] -> []
    | x::l -> match f x with
	None -> mapfilter f l
      |	Some y -> y::mapfilter f l

let rec mapfind : ('a -> 'b option) -> 'a list -> 'b =
  fun f -> function
  | [] -> raise Not_found
  | x::l -> match f x with
      | None -> mapfind f l
      | Some y -> y

let rec fold_while : ('a -> 'a option) -> 'a -> 'a =
  fun f e ->
    match f e with
    | None -> e
    | Some e' -> fold_while f e'

let fold_for : (int -> 'a -> 'a) -> int -> int -> 'a -> 'a =
  fun f a b e ->
    let res = ref e in
    for x = a to b do
      res := f x !res
    done;
    !res

let fold_for_down : (int -> 'a -> 'a) -> int -> int -> 'a -> 'a =
  fun f a b e ->
    let res = ref e in
    for x = a downto b do
      res := f x !res
    done;
    !res

let rec fold_in_channel : ('b -> string -> 'b) -> 'b -> in_channel -> 'b =
  fun f e ch ->
    try fold_in_channel f (f e (input_line ch)) ch
    with End_of_file -> e

let rec insert : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list =
  fun order x ->
    function
    | [] -> [x]
    | y::ys ->
       if order x y
       then x::y::ys
       else y::insert order x ys

let rec insert_max : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list =
  fun order x ->
    function
      | [] -> [x]
      | y::ys ->
	  if order x y then y::ys
	  else if order y x then insert_max order x ys
	  else y::insert_max order x ys

let rec merge_max : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list =
  fun order l1 l2 ->
    List.fold_left
      (fun res x2 -> insert_max order x2 res)
      l1 l2

(* fold on all ordered pairs of a list *)
let rec fold_pair : ('a -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b =
  fun f l e ->
    match l with
    | [] -> e
    | x1::xs ->
       List.fold_right
         (fun x2 res -> f x1 x2 res)
         xs
         (fold_pair f xs e)

let compare_pair : ('a -> 'a -> int) * ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int =
  fun (comp1,comp2) (a1,a2) (b1,b2) ->
    match comp1 a1 b1 with
    | 0 -> comp2 a2 b2
    | c1 -> c1

let rec scramble : 'a list -> 'a list =
  function
  | [] -> []
  | x::l ->
     let l' = scramble l in
     if Random.int 2 = 0
     then x::l'
     else l'@[x]

let rec scrambles : 'a list -> int -> 'a list =
  fun l -> function
  | 0 -> l
  | n -> scrambles (scramble l) (n-1)

let rec sub_list l pos len =
  if pos = 0
  then
    if len = 0
    then []
    else
      match l with
      | [] -> []
      | x::xs -> x::sub_list xs 0 (len-1)
  else
    match l with
    | [] -> []
    | x::xs -> sub_list xs (pos-1) len


(* utilities on streams *)

let rec stream_map f = parser
  | [<'x; str>] -> [<'f x; stream_map f str>]
  | [<>] -> [<>]

(* time *)

let utime () : float = (Unix.times ()).Unix.tms_utime (* in seconds *)

(* for profiling *)

let tbl_prof : (string,(int * float * float)) Hashtbl.t = Hashtbl.create 100

let prof : string -> (unit -> 'a) -> 'a =
  fun s f -> (* f () *)
(* print_string ("<"^s^":"); flush stdout; *)
    let t1 = (Unix.times ()).Unix.tms_utime in
    let m1 = Gc.allocated_bytes () (* float_of_int (Gc.stat ()).Gc.live_words *) in
    let y = f () in
    let t2 = (Unix.times ()).Unix.tms_utime in
    let m2 = Gc.allocated_bytes () (* float_of_int (Gc.stat ()).Gc.live_words *) in
    let n, t, m = try Hashtbl.find tbl_prof s with Not_found -> 0, 0., 0. in
    Hashtbl.replace tbl_prof s (n+1, t +. (t2 -. t1), m +. (m2 -. m1));
(* print_string (s^">\n"); flush stdout; *)
    y

(* utilities on files *)

(* found at http://pauillac.inria.fr/~remy/poly/system/camlunix/fich.html#toc13 *)
let file_copy input_name output_name =
  let buffer_size = 8192 in
  let buffer = String.create buffer_size in
  let fd_in = Unix.openfile input_name [Unix.O_RDONLY] 0 in
  let fd_out = Unix.openfile output_name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
  let rec copy_loop () =
    match Unix.read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r -> ignore (Unix.write fd_out buffer 0 r); copy_loop () in
  copy_loop ();
  Unix.close fd_in;
  Unix.close fd_out

let string_of_file (filename : string) : string =
  let ch = open_in filename in
  let buf = Buffer.create 10000 in
  fold_in_channel
    (fun () line -> Buffer.add_string buf line; Buffer.add_char buf '\n')
    () ch;
  close_in ch;
  Buffer.contents buf

(* probabilities *)

open Num

let comb_tbl : (int*int,num) Hashtbl.t = Hashtbl.create 10000
let rec comb (k,n) =
  if k > n or n < 0 then Int 0
  else if k = n or k = 0 then Int 1
  else if k > n / 2 then comb (n-k,n)
  else
    try Hashtbl.find comb_tbl (k,n)
    with Not_found ->
      let res = comb (k,n-1) +/ comb (k-1,n-1) in
      Hashtbl.add comb_tbl (k,n) res;
      res

let chance_eq_num (r,w) (k,n) =
  comb (k,r) */ comb (n-k,w-r) // comb (n,w)

let chance_eq (r,w) (k,n) = prof "chance_eq" (fun () ->
  float_of_num (chance_eq_num (r,w) (k,n)))

let chance_ge_num (r,w) (k,n) =
  let res = ref (Int 0) in
  for tp = k to r do
    for fp = n-k downto 0 do
      res := !res +/ chance_eq_num (r,w) (tp,tp+fp)
    done
  done;
  !res

let chance_ge (r,w) (k,n) = prof "chance_ge" (fun () ->
  float_of_num (chance_ge_num (r,w) (k,n)))

(* external applications *)

let xemacs filename pattern =
  ignore (Sys.command ("xemacs -eval '(progn (find-file \""^filename^"\") (search-forward \"" ^ pattern ^ "\"))' &"))

let mozilla url =
  ignore (Sys.command ("mozilla -remote \"openurl(" ^ url ^ ")\""))

let gqview filename =
  ignore (Sys.command ("gqview \"" ^ String.escaped filename ^ "\" &"))

let xmms filename =
  ignore (Sys.command ("xmms \"" ^ String.escaped filename ^ "\" &"))

let cyg2win path = (* convert a Cygwin path to a Windows path *)
  match Str.split (Str.regexp "/") path with
  | "cygdrive"::drive::l -> String.uppercase drive ^ ":\\" ^ String.concat "\\" l
  | l -> "C:\\cygwin\\" ^ String.concat "\\" l

let acdsee_cygwin filename =
  ignore (Sys.command ("/cygdrive/c/Program\\ Files/ACD\\ Systems/ACDSee\\ Trial\\ Version/ACDSee.exe /v \"" ^ cyg2win filename ^ "\" &"))

let irfanview_cygwin filename =
  ignore (Sys.command ("/cygdrive/c/Program\\ Files/IrfanView/i_view32.exe \"" ^ cyg2win filename ^ "\" &"))

let irfanslideshow_cygwin filename =
  ignore (Sys.command ("/cygdrive/c/Program\\ Files/IrfanView/i_view32.exe /slideshow=\"" ^ cyg2win filename ^ "\" &"))

let winamp_cygwin filename =
   ignore (Sys.command ("/cygdrive/c/Program\\ Files/Winamp/winamp.exe \"" ^ cyg2win filename ^ "\" &"))
