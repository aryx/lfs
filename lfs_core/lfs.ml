open Ofullcommon

let copyright () = pr2
"LFS, the Logic File System -- Yoann Padioleau.
Copyright (C) 2000-2008  University de Rennes 1, Institut National de
Recherche en Informatique et en Automatique (INRIA).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

LFS homepage : http://aryx.kicks-ass.org/~pad/software.php
Contact author : yoann.padioleau@gmail.com
"

let copyright_short () = pr2
"LFS, the Logic File System.
Copyright (C) 2000-2008  University de Rennes 1, IRISA, INRIA, France.

LFS comes with ABSOLUTELY NO WARRANTY.  This is free software and you
are welcome to redistribute it under certain conditions.  For details,
see the GNU General Public License.
"

(*****************************************************************************)
(* Data Structures *)
(*****************************************************************************)
type property = Prop of string

(* estet: cant anymore have Obj cos want use interval lib for extension *)
type objet    = (* Obj of *) identity
     and identity = int

type logic    = (property -> property -> bool)  (*  mean: a |= b ? *)


type context = {
  logic_cache: iproperty ograph;
  extensions: (iproperty, objet oset) oassoc;

  conv_iprop: (iproperty, property) oassoc;
  conv_prop:  (property, iproperty) oassoc;
    (* IFNOT OPT3   _O_: (objet * iproperty set) oset; *)
}
and iproperty = Iprop of int


type formula =
  | Single of property
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

(*---------------------------------------------------------------------------*)
type file = {
  filename: filename;
  extrinsic: iproperty set;
  intrinsic: iproperty set;
  fcontent: fakecontent;
  }
  and filename = string
  and filecontent = string
  (* Either the content in core lfs, either id from where we can get the path
   * to content in real lfs. So have in mind fakecontent = filecontent
   * when work in core lfs. If want print a beautiful code,  just do a
   * s/fake/file, and s/core.*__(.+)/$1/
   *)
  and fakecontent = Core of filecontent | Real of objet

type idfile = objet

type transducer  = (fakecontent -> property set)
(* effect!: on w,  could do it other way cos less prop, but just to
 * be consistent with adv_itransducer *)
type itransducer = (fakecontent -> iproperty set)

(*---------------------------------------------------------------------------*)
type part = {
  fromfile: idfile;
  pdescription: iproperty set;
  pcontent: partcontent;
  }
  and partcontent = string

type idpart = objet

type adv_transducer  = (partcontent list -> (property set) list)
(* effect!: does some on w, cos of iprop. Cant keep all the strings in mem,
 * and in descr when use disk *)
type adv_itransducer = (partcontent list -> (iproperty set) list)

type parts_info = {
  parts_info: (idpart, part) oassoc;
  line_to_part: idpart oarray;
  lines_synchro: int oset;
  (* multi level synchro, the pair is (level, state) *)
  synchroinfo: (int, (int * property list)) oassoc;
  }

(*--------------------------------------------------------------------------*)
type world = {
  graphp: iproperty ograph;
  prop_iprop: (property, iproperty) oassoc;
  iprop_prop: (iproperty, property) oassoc;

  (* fast_logic,  value are in vattr format, and key is the attr
   * (could also use iproperty for key) *)
  cache_is_formula: ((property, property oset) oassoc);

  files: (idfile, file) oassoc;
  extfiles: (iproperty, idfile oset) oassoc;

  parts: (idfile, parts_info) assoc;
  extparts: (iproperty, idpart oset) oassoc;
  partsfile: (idpart, idfile) oassoc;

  (* used only in core lfs (or to put solver in builtin) *)
  plugins: (idfile, plugin) assoc;

  pwd_history: (formula * whichmode * misc_options) stack;
  }
  and whichmode = Files | Parts
  and plugin    =
         | Logic of logic
         | Trans of transducer
         | AdvTrans of adv_transducer
  and misc_options = {
    ls_mode:    ls_mode;
    mkdir_mode: mkdir_mode;
    cd_mode:    cd_mode;
    view_mode:  view_mode;
   }
     and ls_mode    =
       | Strict | Relaxed
       | Ext | Int
       | Best | CA
       | Classic | Parents
       (* less: could also do ExtBoth, FileNumbers of int, ...  *)
     and mkdir_mode = Normal | Compat
     and cd_mode    = AllowJump | NoJump
     and view_mode  =
        | SingleV of property
        | OrV  of property * property
        | NotV of property
        | SpecialV

let root = Prop "true"


(*---------------------------------------------------------------------------*)
let (string_of_prop: property -> string) = fun (Prop s) -> s
let s_of_p = string_of_prop

let (string_of_descr_obj: objet -> world -> string) = fun o w ->
  let file = w.files#assoc o in
  (file.extrinsic $+$ file.intrinsic)
  +> List.map (fun ip -> string_of_prop (w.iprop_prop#assoc ip))
  +> concat "/"

let (string_extr_of_descr_obj: objet -> world -> string) = fun o w ->
  let file = w.files#assoc o in
  (file.extrinsic)
  +> List.map (fun ip -> string_of_prop (w.iprop_prop#assoc ip))
  +> concat "/"




let (lfs_mode: world -> whichmode) = fun w -> (w.pwd_history +> top +> snd3)

let (context: world -> context) = fun w ->
  {
    logic_cache = w.graphp;
    extensions =
      if lfs_mode w = Files
      then w.extfiles
      else w.extparts
      ;
    (* IFNOT OPT3  _O_ =  *)
    (* IFNOT OPT3    if is_files_mode w *)
    (* IFNOT OPT3    then w.files#fold (fun acc (idf, f) -> acc#add (idf, f.intrinsic $+$ f.extrinsic)) (new osetb Setb.empty) *)
    (* IFNOT OPT3    else w.parts +> fold (fun acc (id, info) ->  *)
    (* IFNOT OPT3            info.parts_info#fold (fun acc (idp, p) -> acc#add (idp, p.pdescription)) acc *)
    (* IFNOT OPT3            ) (new osetb Setb.empty) *)
    conv_iprop = w.iprop_prop;
    conv_prop  = w.prop_iprop;
  }

(*---------------------------------------------------------------------------*)
(* Core/Real stuff *)
let _realfs = ref false

let (core_get_fcontent__id: (fakecontent -> filecontent) ref)   = ref
    (function
    | Core content -> content
    | Real _ -> raise Impossible
    )
let (core_get_size_fcontent__slength: (fakecontent -> string) ref) = ref
    (function
    | Core content -> slength content +> i_to_s
    | Real _ -> raise Impossible
    )
let (core_get_date_fcontent__today: (fakecontent -> Unix.tm) ref) = ref
    (function
    | Core content ->
      (* Why this date ? Because only way to work in core mode. *)
        Unix.time () +> Unix.localtime
    | Real _ -> raise Impossible
    )

(* normally called in mkfile and so have no content yet if realfs *)
let (core_set_fcontent__fst: ((fakecontent * identity) -> fakecontent) ref) = ref
    (function
    | ((Core content), o) -> Core content
    | (Real _, _) -> raise Impossible)
(* this time have really a content *)
let core_update_fcontent__fst = ref
    (function (content, o) -> (Core (content())))

(*---------------------------------------------------------------------------*)
let (stat_world: world -> unit) = fun w ->
  let iroot = w.prop_iprop#assoc root in
  eprintf "Nbattr: %d (nodes). %d (extfiles), %d (extparts)\n"
    (w.graphp#nodes)#length
    w.extfiles#length
    w.extparts#length
    (* can check also conv_* *)
  ;

  eprintf "Nbobj: %d (extfile root), %d (files),  %d (extparts root), %d (partsfile)\n"
    (w.extfiles#assoc iroot)#cardinal
    w.files#length
    (try ((w.extparts#assoc iroot)#cardinal)
      with Not_found -> -1)
    (w.partsfile#length)
    (* Exn only with OPT3. With OPT2 no pb cos dont call empty_extparts
     * so get the old extparts in mem which have iroot.
     *)
  ;

  let count_extr = w.files#tolist +> List.map (fun (idf, file) ->
    List.length file.extrinsic)
    +> Common.sum in
  let count_intr = w.files#tolist +> List.map (fun (idf, file) ->
    List.length file.intrinsic)
    +> Common.sum in
  let count_part = w.partsfile#tolist +> List.map (fun (idp, idf) ->
    (((w.parts +> assoc idf).parts_info#assoc idp).pdescription
      +> List.length))
    +> Common.sum in

  eprintf "Attr/line: %d (files), %d (parts), %d (extrinsic), %d (intrinsic) \n"
    ((count_extr + count_intr)   /! (w.files#length))
    (count_part /! (w.partsfile#length))
    (count_extr /! (w.files#length))
    (count_intr /! (w.files#length));

  (*
  let count = w.partsfile#fold (fun a (idp, idf) ->
    a +
      (upward_props
          ((w.parts +> assoc idf).parts_info#assoc idp).pdescription
          w.graphp
        +> length))
    0 in
  eprintf "Full Attr/line: %d (parts)\n"
    (count / (w.partsfile#length + 1))
  *)
  ()



(*---------------------------------------------------------------------------*)
let rec (check_world: world ->  unit) = fun w ->

  let iroot = w.prop_iprop#assoc root in

  (* lfs_robust: cant have multiple logic engine for same attr
   *  (forall ext(logic:xxx) = 1 (or 0))
   * lfs_robust: lfs_check:  each prop in logic cache is a child of root
   *  (accessible prop)
   *)

  pr2 "Checking logic cache";
  Common.execute_and_show_progress (w.prop_iprop#length) (fun k ->
   w.prop_iprop#iter (fun (p, ip) ->
    try
      k ();
      let _parents =  w.graphp#predecessors ip in
      let _children = w.graphp#successors ip in
      (* let allparents = upward_props parents#tolist w.graphp in *)
      ()
    with _ -> pr2 ("check_world: pb with prop " ^ (string_of_prop p))
    );
  );

  pr2 "Checking validity of extensions";
  Common.execute_and_show_progress (w.extfiles#length) (fun k ->
   w.extfiles#iter (fun (p, oi) ->
    k ();
    try oi#invariant ()
    with _ ->
      pr2 (spf "check_world: pb with extension of %s = %s "
              (string_of_prop (w.iprop_prop#assoc p))
              (oi#to_string ()))
   );
  );

  pr2 "Checking inlining of extensions";
  (* note: do also the following invariant = union extension children
   * include in extension parent (cos inline => bigger parents) *)
  Common.execute_and_show_progress (0) (fun k ->
   let rec dfs = function
    | [] -> ()
    | p::ps ->
        k();
        try
          let children = w.graphp#successors p in
          let oip = w.extfiles#assoc p in
          begin
            children#iter (fun q ->
              try (
                let oiq = w.extfiles#assoc q in
                if not (oiq $<<=$ oip)
                then pr2 ("check_world: pb with extension of " ^
                             (string_of_prop (w.iprop_prop#assoc q)) ^
                             " not included in extension of " ^
                             (string_of_prop (w.iprop_prop#assoc p)) ^
                             " diff = " ^
                             ((oiq $--$ oip)#to_string()) ^
                             "")
                )
              with _ -> pr2 ("check_world: pb with prop " ^
                                (string_of_prop (w.iprop_prop#assoc q)))


            );
            dfs children#tolist; (*  in depth *)
            dfs ps               (*  in breath *)
          end
        with _ ->
          pr2 ("check_world: pb with prop " ^ s_of_p (w.iprop_prop#assoc p));
          raise Not_found


  in
  (* todo: should check also that children of iroot are in iroot
   * (cf bug with lose flush counter file => get empty ext for iroot but
   * not detected via check_world)
   *)
   dfs [iroot];
  );


  if (w.graphp#nodes)#length <> w.extfiles#length
  then pr2 "check_world: pb, not same number of attrs (extfile)";

  if (w.graphp#nodes)#length <> w.prop_iprop#length
  then pr2 "check_world: pb, not same number of attrs (prop_iprop)";

  if (w.graphp#nodes)#length <> w.iprop_prop#length
  then pr2 "check_world: pb, not same number of attrs (iprop_prop)";


  if (w.extfiles#assoc iroot)#cardinal <>  w.files#length
  then pr2 "check_world: pb, not same number of objects";

  let max_iprop = ref 0 in
  let iprop_to_int = function Iprop i -> i in

  pr2 "Checking prop_iprop";
  Common.execute_and_show_progress (w.prop_iprop#length) (fun k ->
   w.prop_iprop#iter (fun (p, ip) ->
    k ();
    try
      let ip' = w.prop_iprop#assoc p in
      Common.assert_equal ip' ip;
      let _parents =  w.graphp#predecessors ip in
      let _children = w.graphp#successors ip in
      let _ext = w.extfiles#assoc ip in
      max_iprop := max !max_iprop (iprop_to_int ip);
      ()
    with Not_found ->
      pr2 ("check_world: pb with property:" ^ (string_of_prop p))
   );
  );

  pr2 "Checking iprop_prop";
  Common.execute_and_show_progress (w.iprop_prop#length) (fun k ->
    w.iprop_prop#iter (fun (ip, p) ->
      k();
      try
        let p' = w.iprop_prop#assoc ip in
        Common.assert_equal p' p;
        let _parents =  w.graphp#predecessors ip in
        let _children = w.graphp#successors ip in
        let _ext = w.extfiles#assoc ip in
        ()
      with Not_found ->
        pr2 ("check_world: pb with property:" ^ (string_of_prop p))
    );
  );

  pr2 "Checking some equality of global numbers (cardinality, ...)";
  if !max_iprop > !Common._counter2
  then begin
    pr2 (spf "check_world: pb counter2 is not good, (max = %d) != (counter2 = %d)" !max_iprop !Common._counter2);
    Common._counter2 := !max_iprop;
    pr2 (spf "check_world: I am adjusting it, now here is the value of counter2 %d" !Common._counter2);
  end;
  hook_action_check_world +> Common.run_hooks_action w;
  ()
and hook_action_check_world = ref [fun p  -> log3 "hook"]



(*---------------------------------------------------------------------------*)
let (new_object: unit -> objet)     = fun () ->
  (*Obj*) (counter ())  (* todo: freeObj *)

(* cos dont want holes in files/xxx  *)
let (new_object_part: unit -> objet)     = fun () ->
  (*Obj*) (counter3 ())  (* todo: freeObj *)

let _ = Common._counter3 := 2

(* note: if want go to test old version with only prop (to see if
 * really opt), make new_iprop take in param property and make
 * type iproperty = property
 *)
let (new_iprop:  unit -> iproperty) = fun () ->
  Iprop   (counter2 ()) (* todo: freeProp *)

(*---------------------------------------------------------------------------*)
(* oassocb or oassoch, not that much different.
 * old: but may have to order o if use oassoch, otherwise the insert
 * will be slow (cache miss), only for partsinfo and partinfo dont use
 * emptyab now.
 *)
let emptyab () = new oassoch []
let emptysb () = new osetb Osetb.empty

(* coupling: if change, then must change in real-lfs if use bdb,
 * cos marshaller are different *)
let (empty_ext: unit -> objet oset) = fun () -> new oseti []
  (* IFNOT OPT5 let empty_ext () = new osetb Setb.empty *)

(* old: (use old: tag  cos now no more cache miss, even if assoch)
 * note that oassocb sux with this kind of data, put on disk via bdb is
 * better than in mem with oassocb :)
 * note that if oassoch, when cd parts on big file, then have many cache
 * miss with seti, cos the o and so #iter are not ordered :( seems that with
 * oassocbtree, the #iter is ordered.
 * subtil: in fact if marshal the key (as it is the case), then it is
 * ordered for berkeley otherwise then the order is
 * 1 10 100 11 ... 2 21 22 ...  and so get sometimes cache
 * miss (cf ocamlbdb/test.ml for testing)
 *)
let (empty_partsinfo: (unit -> (idpart, part) oassoc) ref) =
  ref (fun () -> new oassoch [])
let (empty_extparts:  (unit -> (iproperty, objet oset) oassoc) ref) =
  ref (fun () -> new oassoch [])

(*---------------------------------------------------------------------------*)

let iroot = new_iprop () (* todo: cause of bug ? *)

let default_ls_mode = ref Strict
let default_mode () = {
  ls_mode    = !default_ls_mode;
  mkdir_mode = Normal;
  cd_mode    = AllowJump;
  view_mode  = SingleV root;
}

let default_world () = {
  graphp = (new ograph2way (emptyab()) (emptyab()) emptysb)#add_node iroot;
  prop_iprop = (emptyab())#add (root, iroot);
  iprop_prop = (emptyab())#add (iroot, root);
  cache_is_formula = (emptyab());

  files = emptyab();
  extfiles = (emptyab())#add (iroot, empty_ext ());
  parts = empty_list;
  extparts = (emptyab())#add (iroot, empty_ext ());
  partsfile = emptyab();

  plugins = empty_list;
  pwd_history = push ((Single root), Files, default_mode()) empty_list
}



(*---------------------------------------------------------------------------*)
let (big_union_ext: ('a -> 'b oset) -> 'a oset -> 'b oset) = fun f xs ->
  xs#fold (fun acc e -> acc#union (f e)) (empty_ext())

let (big_unionb: ('a -> 'b oset) -> 'a oset -> 'b oset) = fun f xs ->
  xs#fold (fun acc e -> acc#union (f e)) (new osetb (Osetb.empty))



let (upward_props: iproperty set -> iproperty ograph -> iproperty set) =
 fun xs graph ->
  (graph#ancestors ((emptysb())#fromlist xs))#tolist (*  OPT4 *)
       (* IFNOT OPT4  xs *)

(* used only with old version of lfs (with less optimisation) *)
let (downward_props: iproperty set -> iproperty ograph -> iproperty set) =
 fun xs graph ->
  (graph#children ((emptysb())#fromlist xs))#tolist




let rec (properties_of_formula: formula -> property set) = function
  | Single x -> set [x]
  | (And (f1, f2) | Or (f1, f2)) ->
      (properties_of_formula f1) $+$ (properties_of_formula f2)
  | Not f -> properties_of_formula f

let hook_is_special_prop = ref (fun p k -> false)
let is_special_prop = fun p ->
  !hook_is_special_prop p (fun p -> failwith "no continuation here")


let rec (properties_conj_of_formula: formula -> property set) = function
  | Single x -> if is_special_prop x then set [] else   set [x]
  | And (f1, f2) ->
      (properties_conj_of_formula f1) $+$ (properties_conj_of_formula f2)
  | Or (f1, f2) -> set []
  | Not f -> set []


let rec (is_conjunction: formula -> bool) = function
  | Single _ -> true
  | And (f1, f2) -> is_conjunction f1 && is_conjunction f2
  | ((Or  _)|(Not _)) -> false

let (exist_prop: world -> property -> bool) = fun w p ->
    w.prop_iprop#haskey p
    (* old: p $??$ w.graph#nodes
     * (=> if want get rid iprop, have to have fast #nodes operation
     * (need get back to old ograph2way)
     *)


(*****************************************************************************)
(* Algorithms *)
(*****************************************************************************)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Ls *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

let rec (ext2: formula -> context -> objet oset) = fun f ctx ->
  match f with
  | Single p ->
      (fun _ -> ctx.extensions#assoc (ctx.conv_prop#assoc p))
      +> !hook_compute_ext (p, ctx)  (*  OPT4 *)
  | And (f1, f2) -> (ext2 f1 ctx) $**$ (ext2 f2 ctx)
  | Or  (f1, f2) -> (ext2 f1 ctx) $++$ (ext2 f2 ctx)
  | Not f -> (ctx.extensions#assoc iroot) $--$ (ext2 f ctx) (*  OPT3 *)
        (* IFNOT OPT4 | Single q -> downward_props [ctx.conv_prop#assoc q] ctx.logic_cache ##+> (fun xs -> pr2 (i_to_s (length xs));xs) ##DEBUG ##OPT3  *)
        (* IFNOT OPT4      +> fold (fun acc p -> acc $++$ (try (ctx.extensions#assoc p) with _ -> empty_ext ())) (empty_ext())  ##OPT3    *)
        (* IFNOT OPT3 | Single q ->  *)
        (* IFNOT OPT3       let q = ctx.conv_prop#assoc q in *)
        (* IFNOT OPT3       let down = downward_props [q] ctx.logic_cache in *)
        (* IFNOT OPT3       ctx._O_#fold  (fun acc (o,ps) ->   *)
        (* IFNOT OPT3    if (q $?$ ps ||  (down +> exists (fun p -> p $?$ ps)))  *)
        (* IFNOT OPT3    then acc#add o else acc *)
        (* IFNOT OPT3    ) (emptysb()) *)
        (* IFNOT OPT3  | Not f ->        raise Todo *)
and (hook_compute_ext: ((property * context) -> ((property * context) -> objet oset) -> objet oset) ref) =
  ref (fun (p,ctx) k ->
      log3 ("ext:" ^ string_of_prop p);
      k (p,ctx)
  )
let ext a b =
  Common.profile_code "Lfs.ext" (fun () -> ext2 a b)


let (dirs2: formula -> misc_options -> context -> (property * int) oset) =
 fun f options ctx ->
  let graph = ctx.logic_cache in
  let ois = ext f ctx in
  log3 "dirs";
  match options.ls_mode with
  | Ext -> emptysb()
  | Classic ->
     let x = (match f with ((Single x)|(And (Single x, _))) -> x   | _ -> root)
     in
     (graph#successors (ctx.conv_prop#assoc x)) +> mapo (fun ip ->
       let newois =
         (try (ctx.extensions#assoc ip)
           with Not_found -> empty_ext ()) $**$ ois
       in
        (ctx.conv_iprop#assoc ip, newois#length)
       ) (emptysb())
  | Parents ->
     let x = (match f with ((Single x)|(And (Single x, _))) -> x   | _ -> root)
     in
     (graph#predecessors (ctx.conv_prop#assoc x)) +> mapo (fun ip ->
        let newois =
          (try (ctx.extensions#assoc ip)
          with Not_found -> empty_ext ()) $**$ ois
        in
        (ctx.conv_iprop#assoc ip, newois#length)
       ) (emptysb())
  | _ ->

   let visited = hcreate () in
   let dirs_size = ref (emptysb()) in

   let rec dfs  = fun ps ->
     ps#iter (fun p ->
       if not (visited +> hmem p) then begin
         visited +> hadd (p, true);
         log3 ("dirs:dfs: visisting "^string_of_prop (ctx.conv_iprop#assoc p));

         (*  we do a try cos when cd parts some props are no more defined
          * coupling: same than in ext.
          * doc: why we dont call ext instead of having this duplication
          * of code ? justement a cause de ce que dit avant ? du fait que
          * faut faire try, et que dans ext y'a pas de try
          * car veut garder un ext propre et c'est aussi et surtout une
          * question d'optimisation, car refait pas tout depuis le debut *)

         let newois =
           (try (ctx.extensions#assoc p)
             with Not_found -> empty_ext ()) $**$ ois
         in (* OPT4 *)
           (* IFNOT OPT4    let newois = (downward_props [p] ctx.logic_cache ##+> (fun xs -> pr2 (i_to_s (length xs));xs) ##DEBUG ##OPT3 *)
           (* IFNOT OPT4     +> fold (fun acc p -> acc $++$ (try (ctx.extensions#assoc p) with _ -> empty_ext ())) (empty_ext()))  $**$ ois in ##OPT3 *)
           (* IFNOT OPT3 let newois = ext (Single (ctx.conv_iprop#assoc p)) ctx $**$ ois in *)

         (* ESTET? duplication of code         *)
         match (newois#length,   ois#length) with
         | (newl, oldl) when newl = oldl && newl > 1 ->
             (match options.ls_mode with
             | Strict -> dfs (graph#successors p)
             | Relaxed ->
                 let props =
                   ctx.logic_cache#ancestors
                     ((emptysb())#fromlist
                         ((properties_conj_of_formula f)
                           +> filter (fun p -> not (is_special_prop p)) (* useless now cos done in conj_of_formula *)
                           +> map ctx.conv_prop#assoc))
                 in
                 if not (p $??$ props)
                 then dirs_size := !dirs_size#add (p, newl)
                 else dfs (graph#successors p)
             | _ -> raise Impossible
             )
         | (newl, oldl) when 0 < newl && newl < oldl ->
             (* TODO but strange behaviour with views (and relaxed?) cos
              * if ext:c |= type:Program  and go in ext:^, then dont see
              *ext:c :( )
              * faudrait verifier si dans vue, si les parents seront
              * atteint (descendant de start_point (mais couteux ?))
              *    interaction avec .relaxed aussi ?
              * ## we want the max, the next parent will do the job
              * (if he has to do the job)
              *  if ((graph#predecessors p)#exists
              *    (fun p -> not (visited +> hmem p))) ||
              *     (graph#predecessors p)#exists (fun p -> p $??$ !dirs) subtil: DOC
              *
              *  then ()
              *  else
              *)

             (* when cd .relaxed/function:|match:, rentre que dans
              * function:,  car c le seul ou newl = oldl.
              *  en fait kan newl < oldl faut aussi des fois aller plus loin.
              * mais bon du coup si tu fais cd title:xx|title:yy apres il
              * te les propose plus :( )
              *)
             let props = ctx.logic_cache#ancestors
               ((emptysb())#fromlist
                   ((properties_conj_of_formula f)
                     +> filter (fun p -> not (is_special_prop p)) (* useless now cos done in conj_of_formula *)
                     +> map ctx.conv_prop#assoc))
             in
             if not (p $??$ props)
             then dirs_size := !dirs_size#add (p, newl)
             else dfs (graph#successors p)

         | _ -> visited +> hadd (p, true)
       end
     )
   in
   begin
     (match options.view_mode with
     | SingleV p -> dfs (graph#successors (ctx.conv_prop#assoc p))
     | OrV (p1, p2) -> dfs ( (graph#successors (ctx.conv_prop#assoc p1))
                               $++$
                               (graph#successors (ctx.conv_prop#assoc p2))
                           )
     | NotV p -> let ip = (ctx.conv_prop#assoc p) in
                 dfs (graph#brothers ip)

     | SpecialV ->
         let props =
           [
             Prop "domain";
             Prop "original";
             Prop "props-classic";
             (*Prop "props-generic";*)
             Prop "props-misc";
             Prop "props-quality";
             Prop "props-type2";
           ]
         in
         let allips =
           props +> List.map (fun p ->
             graph#successors (ctx.conv_prop#assoc p)
           )
         +> List.fold_left ($++$) (new osetb (Osetb.empty))
         in
         dfs allips
     );
     !dirs_size +> mapo (fun (x,i) -> (ctx.conv_iprop#assoc x, i)) (emptysb())
   end

let dirs a b c =
  Common.profile_code "Lfs.dirs" (fun () -> dirs2 a b c)


let (objects: formula -> context -> (property * int) oset -> objet oset) =
 fun pwd ctx dirs ->
  (* oldsimple:
   *  ((ext pwd ctx) $--$ dirs +> big_union_ext (fun (p,i) ->
   *  ext (And (pwd, Single p)) ctx))#tolist
   *
   * note: todo: not sure this code better now that have good union/diff
   * for seti
   *)
  log3 "objects";
  let objs = ext pwd ctx in
  let h = new oassoch [] in
  objs#iter (fun o -> ignore(h#add (o, 0)));
  dirs#iter (fun (p,i) ->
    (* todo: heavy computing ? specially when use the google:xxx, cos
     * the pwd formula can be big
     * todo: => would be better if get passed the oipwd, and do
     * a $**$ with extension (no need try to handle extparts,
     * cos objects only called in Files mode)
     *)
    (ext (And (pwd, Single p)) ctx)#iter (fun o -> ignore(h#add (o,1))));

  h#fold (fun acc (o,i) -> if i = 0 then acc#add o else acc) (emptysb())


(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Logic cache *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*
 * rule: b |= a <=> every model of b is model of a,
 * if one valuation of 'b' is true, then must be true for 'a' too
 * ex:  b |= Top, b&c |= b, b |= b|c,
 * when Top, and add b&c -> must go below
 * when b,   and add b&c -> must go below
 *
 * principle: as ferre, try do minimum number of |=, just go down until
 * not more implied then from this parent, it checks the child that
 * are imply, and then update
 *
 * subtil: find_children: the pb of go below.
 * if d is not created, and make d but there is (b b&d) so
 * when found b; b dont |= d but we must go below
 * is also elsewhere, for ex if we have (Top (b|c b c) d a)
 * and insert c|a !!! then the c is enfouie and c|a and b|c are uncomparable
 * => we dont go below => pb
 * => there might be more childs_under than expected !!
 * todo: perhaps can optimise by having logic that have good property
 * (where |= answer either yes|no|reallyno and that when have reallyno =>
 * dont even have to go below
 *
 *
 * subtil:
 * are there under the parents ? yes !! (cf proof)
 * may they be child from parent of parents ? no !! (cf proof)
 * is there the same pb with parents ? possible parents elsewhere ? no!!
 * (cf proof)
 * sert a aller en dessous des first child found ? no!! (cf proof)
 * there are the more general and the correct one
 *
 * subtil: add_child:
 * it is not       union [x] res
 *  cos you can have first found c, then you find b&c (less general)
 * as in (b|c (b b&c) (c b&c)) when insert c|a you go in b|c
 * then try the first path (b b&c) and find as child b&c
 * then try second path (c b&c) and find c, so as c is more general than b&c
 * you must suppr it
 * before union, you must check that this one is not deduce !!!! (pass1)
 * AND you can do the inverse (pass2)
 *
 * bugfix pb b&d and c|a
 * i tried to fix the pb of b&c and c under c|a with not using an union
 * find_child but by using add_in(good), but i call find_child with
 * the list of child preprocessed (not |=), => c who was a direct child
 * was not in, so when findchild encounter b&c and call add_in
 * there was no c in the biglist => it let b&c => cf my code call findchild
 * with all the child (even those who are |= (as c))
 *
 * insert:
 * if = there must be only 1 parent ? yes!!, easy to prove, only one node
 * per f!!!
 * ordre partial, and the second test make a => b /\ b => a   => b = a
 *
 * bugfix insert:
 * before do each time add and del, but we can have double arrow
 * to a child already inserted (if a simple join)
 *
 * is there reaction en chaine ?? cos we calculate, then update,
 * but is it possible that some update change calculus ?? no
 *
 * opti: could avoid even more call to |=, for instance in add_child because
 * add_child compare formula that were previously inserted so both are know
 * so could look in graph
 * could opti by caching the childall
 *
 *)

let rec (find_parents: property -> logic -> (iproperty ograph * (iproperty, property) oassoc) -> iproperty -> iproperty oset) =
 fun f (|=) (graph, conv_iprop) parent ->
  let children = graph#successors parent in
  let implied = children#filter (fun child -> f |= (conv_iprop#assoc child)) in
  if implied#null
  then (emptysb())#add parent
  else implied +> big_unionb (fun newparent ->
    find_parents f (|=) (graph, conv_iprop) newparent)

let rec (find_children: property -> logic -> (iproperty ograph * (iproperty, property) oassoc) -> iproperty oset -> iproperty oset) =
 fun f (|=) (graph, conv_iprop) children ->
  if children#null then emptysb()
  else
    let (child, others) = (children#getone, children#others) in
    if ((conv_iprop#assoc child) |= f)
    then find_children f (|=) (graph, conv_iprop) others
      +> add_child child (|=) conv_iprop
    else find_children f (|=) (graph, conv_iprop)
      (others $++$ (graph#successors child))

and (add_child: iproperty -> logic -> (iproperty, property) oassoc -> iproperty oset -> iproperty oset) =
 fun child (|=) conv_iprop children ->
   if (children#exists (fun other ->
     (conv_iprop#assoc child) |= (conv_iprop#assoc other)))
   then children
   else (children#filter (fun other ->
     not (conv_iprop#assoc other |= conv_iprop#assoc child)
     ))#add child

(*---------------------------------------------------------------------------*)
let (insert_property: property -> logic -> (iproperty ograph * (iproperty, property) oassoc) -> iproperty -> (iproperty ograph * iproperty * bool)) =
 fun f (|=) (graph, conv_iprop) top ->
  let parents = find_parents f (|=) (graph, conv_iprop) top in
  let candidates = parents +> big_unionb
    (fun parent -> graph#successors parent)
  in
  let children = find_children f (|=) (graph, conv_iprop) candidates in

  if (parents#is_singleton) && ((conv_iprop#assoc (parents#getone)) |= f)
  then (graph, parents#getone, false)
  else
    let g = ref graph in
    let fi = new_iprop () in
    g := !g#add_node fi;
    parents#iter (fun parent ->
      g := !g#add_arc (parent, fi);
      (children $**$ (graph#successors parent))#iter (fun child ->
        g := !g#del_arc (parent, child));
    );
    children#iter (fun child -> g := !g#add_arc (fi, child));
    (!g, fi, true)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Views *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let mark_string________ mark = (".........:" ^ (i_to_s mark))
let mark_regexp________ = "\\.\\.\\.\\.\\.\\.\\.\\.\\.:"

(*---------------------------------------------------------------------------*)
let (view2: formula -> context -> parts_info -> (filecontent * ((int, idpart list) assoc))) =
 fun pwd ctx info ->
  let parts_pwd = ext pwd ctx in
  let all_parts = info.line_to_part in
  log2 (spf "view: pwd = %d, full = %d\n" parts_pwd#length all_parts#length);

  (* oldsimple: used caml string, but n concatenation means quadratic
   * complexity, and with big file, it sux a lot.
   *)
  let content = Buffer.create 16 in
  let marks = ref empty_list in
  let mark = ref 1 in
  let pending = ref empty_list in

  all_parts#iter (fun (_, id) ->
    if id $??$ parts_pwd
    then
      let part = info.parts_info#assoc id in
      if !pending = empty_list
      then Buffer.add_string content part.pcontent
      else begin
        Buffer.add_string content
          ((mark_string________ !mark) ^ "\n" ^ part.pcontent);
        marks +!> insert_assoc (!mark, rev !pending);
        mark := !mark + 1;
        pending := empty_list;
      end
    else pending := id::!pending
   );
  if !pending = empty_list
  then (Buffer.contents content,  !marks)
  else begin
    Buffer.add_string content ((mark_string________ !mark)^ "\n");
    (Buffer.contents content,  insert_assoc (!mark, rev !pending) !marks)
  end
let view a b c =
  Common.profile_code "Lfs.view" (fun () -> view2 a b c)

(*---------------------------------------------------------------------------*)
let (update_view: filecontent -> parts_info -> ((int, idpart list) assoc) -> filecontent) =
 fun newcontent info marks ->
  (* opti: slow tout ca, en plus map c lent *)
  newcontent
    +> Common.lines_with_nl
    +> map (fun  s ->
      if s =~ mark_regexp________
      then
        let mark = regexp_match s ".*:\\([0-9]+\\)" +> s_to_i in
        assoc mark marks
        +> map (fun id -> (info.parts_info#assoc id).pcontent)
        +> unwords
      else s
    )
    +> unwords

(*---------------------------------------------------------------------------*)
type conversion_func =
  (unit -> (property, iproperty) oassoc) *
  (unit -> (iproperty, property) oassoc)

let (create_parts2: idfile -> file -> adv_itransducer -> conversion_func -> parts_info) =
 fun id file trans (conv_prop, conv_iprop) ->

  let parts = Common.profile_code "Lfs.create_parts 1" (fun () ->
    (* futur: not lines but tokens *)
    Common.lines_with_nl (!core_get_fcontent__id file.fcontent))
  in

  let part_prop = Common.profile_code "Lfs.create_parts 2" (fun () ->
    parts +> trans +> Common.zip parts +> Common.index_list )
  in

  (* If use pure data-structure, must get the table that are up to date.
   * Otherwise when will parse the props, the conv_iprop#assoc would fail
   * cos have the old tables must do it each time we modify those tables
   * => each time call transducer
   * needed only for multi-level synchro (cos Prop "synchro" is still here,
   * same id in old conv_prop)
   *)
  let (conv_prop, conv_iprop) = (conv_prop(), conv_iprop()) in

  Common.profile_code "Lfs.create_parts 3" (fun () ->
   part_prop +> fold (fun info ((s, ps), i) ->
    let o = new_object_part () in
    { parts_info = info.parts_info#add (o,
                                        { fromfile = id;
                                          pdescription = ps;
                                          pcontent = s;
                                        }) ;
      line_to_part = info.line_to_part#add (i, o);

      (* do a try cos there may not be a synchro property *)
      lines_synchro = if (try (conv_prop#assoc (Prop "synchro")) $?$ ps with Not_found -> false) then info.lines_synchro#add i else info.lines_synchro;
      (* todo: take time ? goulot ?  *)
      synchroinfo = ps +> fold (fun acc ((Iprop xx) as iprop) ->
        let (Prop s) = conv_iprop#assoc iprop  in
        if s =~ "synchro\\([0-9]\\):\\(.*\\)" then
          let (level, str_state) = matched2 s in
          (*  manage more than one prop in state with convention :: for separate the props *)
          let state = if str_state = "" then [] else let ps = split "::" str_state in ps +> map (fun s -> Prop s) in
          acc#add (i, (s_to_i level, state))
        else acc
        ) info.synchroinfo;
      (* old:       synchroinfo = info.synchroinfo; *)
    }
   ) { parts_info = !empty_partsinfo() ;
       line_to_part = new oarray (length parts) (-1);
       lines_synchro = emptysb();
       synchroinfo = emptyab();
   }
  )

let create_parts a b c d =
  Common.profile_code "Lfs.create_parts" (fun () -> create_parts2 a b c d)

(*---------------------------------------------------------------------------*)
(* return new partsinfo + todel + toadd, quite complicated *)
let (reindex_parts:  idfile -> filecontent -> (file * parts_info) -> adv_itransducer -> conversion_func -> (parts_info * idpart set * idpart set)) = fun idf newc (oldf, oldinfo) trans (conv_prop, conv_iprop) ->

  let newlines, oldlines =
    Common.profile_code "Lfs.reindex_parts lines_with_nl" (fun () ->
      let newlines = lines_with_nl newc in
      let oldlines = lines_with_nl (!core_get_fcontent__id oldf.fcontent) in
      newlines, oldlines
    )
  in

  (* avoiding indexing reduce time, but avoiding rebuilding from scratch
   * partsinfo reduce time too, => update oldinfo
   * (note: works for pure data structure too, this is orthogonal)
   *  (just add obj, cos cos the caller need the info on the todel so
   * the del of obj will be done in caller)
   *)
  let (newline, newsynchro) =
    ref (new oarray (length newlines) (-1)),
    ref (emptysb())
  in

  let must_index = ref empty_list in
  let must_del   = ref empty_list in

  let may_index_del     = ref empty_list in

  let is_in_clean = ref true in

  (newlines, oldlines) +> diff
    (fun newi oldi comparaison ->
      match comparaison with

      | Match ->

          let synchro = oldi $??$ oldinfo.lines_synchro in (* OPT6 *)
          (* IFNOT OPT6 let synchro = false in *)
          (* if it was an synchro in old, then it is still, cos requirment for synchro point *)
          (if synchro then
            (is_in_clean := true;

             !may_index_del +> iter (fun (newi, oldi) ->
               let id = oldinfo.line_to_part#assoc oldi in
               newline := !newline#add (newi, id);
             );

             (* bugfix: may_index_del := [(newi, oldi)]; no, will be done after, otherwise double it *)
             may_index_del := empty_list;
             newsynchro := !newsynchro#add newi;
            ));
          if !is_in_clean
          then (
            (* bugfix: perhaps id will be erased, if forget then will still work but have big leak => may_del *)
            may_index_del   +!> cons (newi, oldi);
          )
          else (must_del +!> cons oldi;
                must_index +!> cons newi;
          )
      | (AnotinB | BnotinA) ->
          must_index +!> (fun xs -> (!may_index_del +> map fst) @ xs);
          must_del   +!> (fun xs -> (!may_index_del +> map snd) @ xs);
          is_in_clean := false;
          may_index_del := empty_list;
          if comparaison = AnotinB
          then must_index +!> cons newi
          else must_del   +!> cons oldi
    );
  (* duplicated from above *)
  !may_index_del +> iter (fun (newi, oldi) ->
    let id = oldinfo.line_to_part#assoc oldi in
    newline := !newline#add (newi, id);
  );

  must_del := rev !must_del;
  must_index := rev !must_index;

  (* TODO use newx and Array.get *)
  let parts = !must_index +> map (fun i -> nth newlines i) in
  let part_prop = Common.profile_code "Lfs.reindex_parts trans" (fun () ->
    parts +> trans +> Common.zip parts +> (fun x -> Common.zip x !must_index)
  )
  in

  (*  used cos want lfs work with pure data-structure too *)
  let (conv_prop, conv_iprop) = (conv_prop(), conv_iprop()) in

  let info = part_prop +> fold (fun info ((s, ps), i) ->
    let o = new_object_part () in
    { parts_info = info.parts_info#add (o,
                                       { fromfile = idf;
                                         pdescription = ps;
                                         pcontent = s;
                                       }) ;
      line_to_part = info.line_to_part#add (i, o);
      lines_synchro =
        if (try (conv_prop#assoc (Prop "synchro")) $?$ ps
            with Not_found -> false)
        then info.lines_synchro#add i
        else info.lines_synchro;
       (* even if dont handle multi, have to parse it, otherwise futur
        * reindex_diff_view2 will get lost *)
      synchroinfo = ps +> fold (fun acc ((Iprop xx) as iprop) ->
        let (Prop s) = conv_iprop#assoc iprop  in
        if s =~ "synchro\\([0-9]\\):\\(.*\\)" then
          let (level, str_state) = matched2 s in
          let state =
            if str_state = ""
            then []
            else let ps = split "::" str_state in
                 ps +> map (fun s -> Prop s)
          in
          acc#add (i, (s_to_i level, state))
        else acc
        ) info.synchroinfo;
       (* old:synchroinfo = info.synchroinfo; *)
      }
   ) { parts_info = oldinfo.parts_info;
       line_to_part = !newline;
       lines_synchro = !newsynchro;
       synchroinfo = emptyab()
  }
  in
  (info,
  !must_index +> map (fun i -> info.line_to_part#assoc i),
  !must_del +>   map (fun i -> oldinfo.line_to_part#assoc i)
  )

(*---------------------------------------------------------------------------*)
let reindex_diff_view = fun id (newc, oldc) (oldf, oldinfo, marks) trans (conv_prop, conv_iprop) ->
  (* let _ = Timing() in *)
  let newlines = lines_with_nl newc in
  let oldlines = lines_with_nl oldc in

  (* let _ = Timing() in *)
  let estimatedlen =  (* robust: bug if duplicate marks, will not have enough space, use growing array! *)
    let nmarks = length marks in
    let nobjs  = marks +> fold (fun a (i, xs) -> a + length xs) 0 in
    length newlines - nmarks + nobjs
  in
  let (newline, newsynchro) = (ref (new oarray estimatedlen (-1)), ref (emptysb())) in

  let must_index = ref empty_list in
  let must_del   = ref empty_list in

  (* i have to pass to transducer some lines, but as synchro != absence-mark, sometimes
     i have to go inside hidden area, and so pass to trans line not in the current view
     so may_index contain Left newi | Right oldi   that reference the line to pass to trans
  *)
  let may_index_del  = ref empty_list in

  let is_in_clean = ref true in

  let addline = ref 0 in
  let newx = Array.of_list newlines in
  let oldx = Array.of_list oldlines in

  (* let _ = Timing() in *)
  let _ = (newlines, oldlines) +> diff
      (fun newi oldi comparaison ->
        (* normally use not at all newi' and oldi', only for the Left of must_index, and to access array of contents *)
        let newi' = newi in
        let oldi' = oldi in
        let newi = newi + !addline in
        let oldi = oldi + !addline in
        (* xi correspond to line in futur complete file, xi' correspond to line in the view, xi' << xi souvent *)
        match comparaison with

        | Match ->
            if (Array.get oldx oldi' =~ mark_regexp________)
            then
              let mark = regexp_match (Array.get oldx oldi') ".*:\\([0-9]+\\)" +> s_to_i in
              let marks = assoc mark marks in
              (* we are in a big match zone *)
              let i = ref 0 in
              marks +> iter (fun idpart ->
                let newi = newi + !i in
                let oldi = oldi + !i in
                (* almost copy paste of beside *)
                (* update juste line and synchro (leave newinfo) *)

                let synchro = oldi $??$ oldinfo.lines_synchro in
                if synchro then
                  (is_in_clean := true;

                   !may_index_del +> iter (fun ((newi,_), oldi) ->
                     (* let id = idpart in *)
                     let id = oldinfo.line_to_part#assoc oldi in
                     newline := !newline#add (newi, id);
                                          );

                   may_index_del := empty_list;
                   newsynchro := !newsynchro#add newi;
                  );
                if !is_in_clean
                then (may_index_del +!> cons ((newi, Right oldi), oldi);)
                else (must_del +!> cons oldi;
                      must_index +!> cons (newi, Right oldi);
                     );
                incr i;
              );
              addline := !addline + length marks - 1;
            else
              let synchro = oldi $??$ oldinfo.lines_synchro in
              (* if it was an synchro in old, then it is still, cos requirment for synchro point *)
              if synchro then
              (is_in_clean := true;

               !may_index_del +> iter (fun ((newi,_), oldi) ->
                 let id = oldinfo.line_to_part#assoc oldi in
                 newline := !newline#add (newi, id);
                                      );

               may_index_del := empty_list;
               newsynchro := !newsynchro#add newi;
              );
              if !is_in_clean
              then (may_index_del +!> cons ((newi, Left newi'), oldi); (* could pass Right oldi, same *) )
              else (must_del +!> cons oldi;
                    must_index +!> cons (newi, Left newi');
                   )

        | (AnotinB | BnotinA) ->
            (match comparaison with
            | AnotinB ->
                if (Array.get newx newi' =~ mark_regexp________ )
                then raise Here
            | BnotinA ->
                if (Array.get oldx oldi' =~ mark_regexp________ )
                then raise Here
            | _ -> raise Impossible
            );
            must_index +!> (fun xs -> (!may_index_del +> map fst) @ xs);
            must_del   +!> (fun xs -> (!may_index_del +> map snd) @ xs);
            is_in_clean := false;
            may_index_del := empty_list;
            if comparaison = AnotinB
            then must_index +!> cons (newi, Left newi')
            else must_del   +!> cons oldi
      ) in

  (* duplicated from above *)
  let _ =   !may_index_del +> iter (fun ((newi,_), oldi) ->
                 let id = oldinfo.line_to_part#assoc oldi in
                 newline := !newline#add (newi, id);
                   ) in

  let _ = (must_del := rev !must_del; must_index := rev !must_index) in

  (* no subtil:  let _ = (!must_del +> List.iter (fun i -> newinfo := !newinfo#delkey (oldinfo.line_to_part#assoc i))) in *)
  (*  this is done by the caller, cant doit here, cos before doing that, we must get the prop of the obj to delete it in extparts *)
  (*  and this part is made in the caller, so this deletion too must be done after *)

  (* let _ = Timing() in *)
  let parts = !must_index +> map (fun (newi, x) ->
    match x with
    | Left newi' -> Array.get newx newi'
    | Right oldi -> (oldinfo.parts_info#assoc (oldinfo.line_to_part#assoc oldi)).pcontent
    )
  in
  let part_prop = parts +> trans +> zip parts +> (fun x -> !must_index +> map fst +> zip x) in

  (*  used cos want lfs work with pure data-structure too *)
  let (conv_prop, conv_iprop) = (conv_prop(), conv_iprop()) in

  let info = part_prop +> fold (fun info ((s, ps), i) ->
      let o = new_object_part () in
      { parts_info = info.parts_info#add (o,
                                          { fromfile = id;
                                            pdescription = ps;
                                            pcontent = s;
                                          }) ;
       line_to_part = info.line_to_part#add (i, o);
       lines_synchro = if (try (conv_prop#assoc (Prop "synchro")) $?$ ps with Not_found -> false) then info.lines_synchro#add i else info.lines_synchro;
       (*  even if dont handle multi, have to parse it, otherwise futur reindex_diff_view2 will get lost *)
       synchroinfo = ps +> fold (fun acc ((Iprop xx) as iprop) ->
          let (Prop s) = conv_iprop#assoc iprop  in
          if s =~ "synchro\\([0-9]\\):\\(.*\\)" then
            let (level, str_state) = matched2 s in
            let state = if str_state = "" then [] else let ps = split "::" str_state in ps +> map (fun s -> Prop s) in
            acc#add (i, (s_to_i level, state))
          else acc
         ) info.synchroinfo;
       (* old:synchroinfo = info.synchroinfo; *)
      }
   ) { parts_info = oldinfo.parts_info; line_to_part = !newline; lines_synchro = !newsynchro; synchroinfo = emptyab();}
  in
  (info,
   !must_index +> map (fun (newi, _) -> info.line_to_part#assoc newi),
   !must_del +>    map (fun i -> oldinfo.line_to_part#assoc i)
  )

(*---------------------------------------------------------------------------*)
let reindex_diff_view2 = fun id (newc, oldc) (oldf, oldinfo, marks) trans conv (conv_prop, conv_iprop) ->

  (* let _ = Timing() in *)
  let newlines = lines_with_nl newc in
  let oldlines = lines_with_nl oldc in

  (* let _ = Timing() in *)
  let estimatedlen =  (* robust: bug if duplicate marks, will not have enough space, use growing array! *)
    let nmarks = length marks in
    let nobjs  = marks +> fold (fun a (i, xs) -> a + length xs) 0 in
    length newlines - nmarks + nobjs
  in
  let (newline, newsynchro, newsynchroinfo) = (ref (new oarray estimatedlen (-1)), ref (emptysb()), ref (emptyab())) in

  let must_index = ref empty_list in
  let must_del   = ref empty_list in

  (* i have to pass to transducer some lines, but as synchro != absence-mark, sometimes
     i have to go inside hidden area, and so pass to trans line not in the current view
     so may_index contain Left newi | Right oldi   that reference the line to pass to trans
  *)
  let may_index_del  = ref empty_list in

  let is_in_clean1 = ref true in
  let is_in_clean2 = ref true in
  let is_in_clean3 = ref true in
  let is_in_clean4 = ref true in
  let props_toadd1  = ref empty_list in
  let props_toadd2  = ref empty_list in
  let props_toadd3  = ref empty_list in

  let addline = ref 0 in
  let newx = Array.of_list newlines in
  let oldx = Array.of_list oldlines in

  (* let _ = Timing() in *)
  let _ = (newlines, oldlines) +> diff
      (fun newi oldi comparaison ->
        (* normally use not at all newi' and oldi', only for the Left of must_index, and to access array of contents *)
        let newi' = newi in
        let oldi' = oldi in
        let newi = newi + !addline in
        let oldi = oldi + !addline in
        (* xi correspond to line in futur complete file, xi' correspond to line in the view, xi' << xi souvent *)
        match comparaison with

        | Match ->
            if (Array.get oldx oldi' =~ mark_regexp________)
            then
              let mark = regexp_match (Array.get oldx oldi') ".*:\\([0-9]+\\)" +> s_to_i in
              let marks = assoc mark marks in
              (* we are in a big match zone *)
              let i = ref 0 in
              marks +> iter (fun idpart ->
                let newi = newi + !i in
                let oldi = oldi + !i in
                (* almost copy paste of beside *)
                (* update juste line and synchro (leave newinfo) *)

            let synchro = (oldi $??$ oldinfo.lines_synchro) || (oldinfo.synchroinfo#haskey oldi) in
            (* if it was an synchro in old, then it is still, cos requirment for synchro point *)
            (if synchro then
              let (level_synchro, state) =
                try oldinfo.synchroinfo#assoc oldi
                with Not_found -> (1, [])
              in
              (
               if (level_synchro = 1) ||
                  ((level_synchro = 2 && !is_in_clean1)) || (*  en fait si pas is_in_clean1, de toute facon le may_index serait vide *)
                  ((level_synchro = 3 && !is_in_clean1 && !is_in_clean2)) ||
                  ((level_synchro = 4 && !is_in_clean1 && !is_in_clean2 && !is_in_clean3))
               then
                 (
                  (* suppose that props inside levelX does not depend content levelX-1 ? *)
                  (*  in fact if start of levelX-1 dirty => will reindex also this part (cos will not have is_in_clean), *)
                  (*  but if end of levelX-1 dirty => will not reindex, so state levelX does not depend content after at least *)
                  (* note also that even if have synchro2 without synchro1 (ex article have not \chapter) then works *)
                  (*  cos at start we are in is_in_clean1 = true. *)
                  !may_index_del +> iter (fun ((newi,_,_) , oldi) ->
                    let id = oldinfo.line_to_part#assoc oldi in
                    newline := !newline#add (newi, id);
                                         );
                  may_index_del := empty_list;
                 );

               if level_synchro = 1 then (is_in_clean1 := true; is_in_clean2 := true; is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 2 then (is_in_clean2 := true; is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 3 then (is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 4 then (is_in_clean4 := true;);

               if level_synchro = 1 then (props_toadd1 := state; props_toadd2 := empty_list; props_toadd3 := empty_list);
               if level_synchro = 2 then (props_toadd2 := state; props_toadd3 := empty_list);
               if level_synchro = 3 then (props_toadd3 := state;);

               newsynchro     := !newsynchro#add newi;
               newsynchroinfo := !newsynchroinfo#add (newi, (level_synchro, state));
              ));


            if (!is_in_clean1 && !is_in_clean2 && !is_in_clean3 && !is_in_clean4)
            then (may_index_del   +!> cons ((newi, (!props_toadd1 @ !props_toadd2 @ !props_toadd3), Right oldi), oldi);)
            else (must_del +!> cons oldi;
                  must_index +!> cons (newi, (!props_toadd1 @ !props_toadd2 @ !props_toadd3), Right oldi);
                 );

                incr i;
              );
              addline := !addline + length marks - 1;
            else


            let synchro = (oldi $??$ oldinfo.lines_synchro) || (oldinfo.synchroinfo#haskey oldi) in
            (* if it was an synchro in old, then it is still, cos requirment for synchro point *)
            (if synchro then
              let (level_synchro, state) =
                try oldinfo.synchroinfo#assoc oldi
                with Not_found -> (1, [])
              in
              (
               if (level_synchro = 1) ||
                  ((level_synchro = 2 && !is_in_clean1)) || (*  en fait si pas is_in_clean1, de toute facon le may_index serait vide *)
                  ((level_synchro = 3 && !is_in_clean1 && !is_in_clean2)) ||
                  ((level_synchro = 4 && !is_in_clean1 && !is_in_clean2 && !is_in_clean3))
               then
                 (
                  !may_index_del +> iter (fun ((newi,_,_) , oldi) ->
                    let id = oldinfo.line_to_part#assoc oldi in
                    newline := !newline#add (newi, id);
                                         );
                  may_index_del := empty_list;
                 );

               if level_synchro = 1 then (is_in_clean1 := true; is_in_clean2 := true; is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 2 then (is_in_clean2 := true; is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 3 then (is_in_clean3 := true; is_in_clean4 := true);
               if level_synchro = 4 then (is_in_clean4 := true;);

               if level_synchro = 1 then (props_toadd1 := state; props_toadd2 := empty_list; props_toadd3 := empty_list);
               if level_synchro = 2 then (props_toadd2 := state; props_toadd3 := empty_list);
               if level_synchro = 3 then (props_toadd3 := state;);

               newsynchro     := !newsynchro#add newi;
               newsynchroinfo := !newsynchroinfo#add (newi, (level_synchro, state));
              ));

            if (!is_in_clean1 && !is_in_clean2 && !is_in_clean3 && !is_in_clean4)
            then (may_index_del   +!> cons ((newi, (!props_toadd1 @ !props_toadd2 @ !props_toadd3), Left newi'), oldi);) (* could pass Right oldi, same *)
            else (must_del +!> cons oldi;
                  must_index +!> cons (newi, (!props_toadd1 @ !props_toadd2 @ !props_toadd3), Left newi');
                 );


        | (AnotinB | BnotinA) ->
            (match comparaison with
            | AnotinB -> if (Array.get newx newi' =~ mark_regexp________ ) then raise Here
            | BnotinA -> if (Array.get oldx oldi' =~ mark_regexp________ ) then raise Here
            | _ -> raise Impossible
            );

            must_index +!> (fun xs -> (!may_index_del +> map fst) @ xs);
            must_del   +!> (fun xs -> (!may_index_del +> map snd) @ xs);

            (* perhaps we have added a new synchro1, in that case have to retransduce *)
            (*  and not just until next synchro2 *)
            (* perhaps have deleted a synchro1, same *)
            if comparaison = BnotinA && oldinfo.synchroinfo#haskey oldi
            then
              (
               let (level, _) = oldinfo.synchroinfo#assoc oldi in
               if level = 1 then (is_in_clean1 := false; props_toadd1 := []; props_toadd2 := []; props_toadd3 := []);
               if level = 2 then (is_in_clean2 := false; props_toadd2 := []; props_toadd3 := []);
               if level = 3 then (is_in_clean3 := false; props_toadd3 := []);
              )
            else
              if oldinfo.synchroinfo#null
              then () (*  means that there will be no pb at all *)
              else
              (
               (*  just call, then there will be again  another call to really index *)
               let xs = [[]] in
               (* TODO1 let xs = trans [Array.get newx newi] in *)
               (*  used cos want lfs work with pure data-structure too *)
               let (conv_prop, conv_iprop) = (conv_prop(), conv_iprop()) in
               let iprops = match xs with [x] -> x | _ -> raise Impossible in
               iprops +> iter (fun iprop ->
                 let (Prop s) = conv_iprop#assoc iprop in
                 if s =~ "synchro\\([0-9]\\):\\(.*\\)" then
                   let (level, _) = matched2 s in
                   let level = s_to_i level in
                   if level = 1 then (is_in_clean1 := false; props_toadd1 := []; props_toadd2 := []; props_toadd3 := []);
                   if level = 2 then (is_in_clean2 := false; props_toadd2 := []; props_toadd3 := []);
                   if level = 3 then (is_in_clean3 := false; props_toadd3 := []);
                             )
             );

            is_in_clean4 := false; (*  the last one *)
            may_index_del := empty_list;
            if comparaison = AnotinB
            then must_index +!> cons (newi, (!props_toadd1 @ !props_toadd2 @ !props_toadd3), Left newi')
            else must_del   +!> cons oldi

      ) in

  (* duplicated from above *)
  let _ =   !may_index_del +> iter (fun ((newi,_,_), oldi) ->
                 let id = oldinfo.line_to_part#assoc oldi in
                 newline := !newline#add (newi, id);
                   ) in

  let _ = (must_del := rev !must_del; must_index := rev !must_index) in

  (* no subtil:  let _ = (!must_del +> List.iter (fun i -> newinfo := !newinfo#delkey (oldinfo.line_to_part#assoc i))) in *)
  (*  this is done by the caller, cant doit here, cos before doing that, we must get the prop of the obj to delete it in extparts *)
  (*  and this part is made in the caller, so this deletion too must be done after *)

  (* let _ = Timing() in *)
  let parts = !must_index +> map (fun (newi, _, x) ->
    match x with
    | Left newi' -> Array.get newx newi'
    | Right oldi -> (oldinfo.parts_info#assoc (oldinfo.line_to_part#assoc oldi)).pcontent
    )
  in

  (* let _ = Timing() in *)
  let part_prop = parts +> trans +> zip parts +> (fun x -> !must_index +> map (fun (x,y,z) -> (x,y)) +> zip x) in

  (* let _ = Timing() in *)
  (*  used cos want lfs work with pure data-structure too *)
  let (conv_prop, conv_iprop) = (conv_prop(), conv_iprop()) in
  (*  normally should do it more often cos after call conv (psadd+> ...) but those prop already exist so dont change anything *)

  let info = part_prop +> fold (fun info ((s, ps), (i, psadd)) ->
      let o = new_object_part () in
      let iprops = psadd +> map_filter conv in
      let ps = iprops @ ps in
      { parts_info = info.parts_info#add (o,
                                          { fromfile = id;
                                            pdescription = ps;
                                            pcontent = s;
                                          }) ;
       line_to_part = info.line_to_part#add (i, o);
       lines_synchro = if (try (conv_prop#assoc (Prop "synchro")) $?$ ps with Not_found -> false) then info.lines_synchro#add i else info.lines_synchro;
       synchroinfo = ps +> fold (fun acc iprop ->
        let (Prop s) = conv_iprop#assoc iprop in
        if s =~ "synchro\\([0-9]\\):\\(.*\\)" then
          let (level, str_state) = matched2 s in
          let state = if str_state = "" then [] else let ps = split "::" str_state in ps +> map (fun s -> Prop s) in
          acc#add (i, (s_to_i level, state))
        else acc
        ) info.synchroinfo;
      }
   ) { parts_info = oldinfo.parts_info; line_to_part = !newline; lines_synchro = !newsynchro; synchroinfo = !newsynchroinfo}
  in
  (* let _ = Timing() in *)
  (info,
   !must_index +> map (fun (newi, _, _) -> info.line_to_part#assoc newi),
   !must_del +>    map (fun i -> oldinfo.line_to_part#assoc i)
  )

(*****************************************************************************)
(* Plug-ins *)
(*****************************************************************************)



(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Logics *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Valued attribute (vattr) are important in LFS; they are used to provide
 * advanced logic. Here we define some accessor to a vattr.
 *)
let regexp_attr =  "^[a-zA-Z_0-9]+:"

let (is_attr:  property -> bool) = fun (Prop s) -> s =~ (regexp_attr ^ "$")
let (is_vattr: property -> bool) = fun (Prop s) -> s =~ (regexp_attr ^ ".+$")

let (value_vattr: property -> property) = fun (Prop s) ->
  Prop (regexp_match s (regexp_attr ^ "\\(.+\\)$"))
let (attr_vattr: property -> property) = fun (Prop s) ->
  Prop (regexp_match s ("\\(" ^ regexp_attr ^ "\\)"))

let _ = Common.example (is_attr  (Prop "toto:"))
let _ = Common.example (is_vattr (Prop "toto:tata"))
let _ = Common.example (value_vattr (Prop "toto:tata") = (Prop "tata"))
let _ = Common.example (attr_vattr  (Prop "toto:tata") = (Prop "toto:"))

(* Cant take just the context, or would need context_of_files, so
 * take the world as a parameter now.
 *)
let rec (find_alogic: world -> property -> logic) =
 fun w ((Prop sprop) as attr) ->

  (* Can we loop ? If put a file in logic:ext, or logic:size, at the
   * moment where create the solver the file is not yet complete ? No
   * cos the file is not yet created for lfs internally, he has no o,
   * no internal identifier. *)
   let plugins =
     try (w.extfiles#assoc (w.prop_iprop#assoc (Prop ("logic:" ^ sprop))))
     with Not_found -> empty_ext()
   in
   assert (is_attr attr);
   assert (plugins#length  <= 1);
   if plugins#null
   then
     (* note: opti: default logic (flat attr) is now handled in check_...
      * old: default logic when no plugins, flat attr
      *)
     raise Not_found
   else begin
     let id = plugins#getone  in
     log2 ("findlogic:" ^ s_of_p attr);
     try (
       match (assoc id w.plugins) with
       | Logic (|=) -> (|=)
       | _ -> failwith "not a logic plugin"
     )
     with Not_found -> !hook_find_alogic id
   end

and (hook_find_alogic: (idfile -> logic) ref) = ref
    (fun id -> raise Not_found)


(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Transducers *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

let transducer_system filename =
  (fun content -> set [
    Prop ("name:" ^ (Common.fileprefix filename));
    Prop ("ext:" ^  (Common.filesuffix filename));
    Prop ("size:" ^ (!core_get_size_fcontent__slength content));
    Prop ("date:" ^ (
      let time = !core_get_date_fcontent__today content in
      Common.string_of_unix_time_lfs time
    ));
  ] )

(* especially useful with multifile
 * ext: name:, make work all the prop from files world => mix => need get
 * the o to get the prop (mais bon verra kan meme tous les file de la racine
 * (et les stats seront plus fausses car plein de prop parasites)
 *
 * old:
 *   (fun contents -> contents +> map (fun c ->
 *       set [(Prop ("file:" ^ filename))] +> map_filter conv))
 * old:
 *   (fun contents -> index_list contents +> map (fun (c, n) ->
 *       set [Prop ("part:" ^ (i_to_s n))]))
 * but sux for synchro cos changed each time
 *)
let adv_transducer_system conv filename file =
  (fun contents -> contents +> map (fun c ->
    file.extrinsic $+$ file.intrinsic))

(*---------------------------------------------------------------------------*)

let rec (find_trans: world -> (property -> iproperty option) -> property -> string -> ((transducer, adv_itransducer) either) set) =
 fun w conv ((Prop sprop) as prop) suffix ->
  if suffix = ""
  then Common.empty_list
  else begin
    (* because the transducer: property may now be managed by a logic,
     * transducer:ml may not exist but still have a plugin cos have a
     * plugin in transducer:BOTTOM, hence the call to check_and_add.
     *)
    ignore(!_check_and_add (Prop (sprop ^ suffix)));
    let candidates =
      try w.extfiles#assoc (w.prop_iprop#assoc (Prop (sprop ^ suffix)))
      with Not_found -> empty_ext()
    in
    let candidates = candidates#tolist in
    candidates +> List.map (fun id ->
      try
        (match assoc id w.plugins with
        | Trans t    ->
            Left t
        | AdvTrans t ->
            Right (fun parts -> t parts +> map (map_filter !_check_and_add))
        | Logic t    -> failwith "not a transducer plugin"
        )
      with Not_found -> !hook_find_transducer id prop conv
    )
  end
and (hook_find_transducer: (idfile -> property -> 'a -> (transducer, adv_itransducer) either) ref)   =
  ref (fun id prop conv -> raise Not_found)
(* estet: do it via pointer func tricks cos dont want move this section
 * after check_and... and def of w *)
and (_check_and_add: (property -> (iproperty option)) ref) =
  ref (fun (Prop s) -> failwith "is defined after")


(* todo? have a del_prop ? *)
let hook_action_add_prop =    ref [fun p  -> log3 "hook"]


let (valid_prop: world -> property -> bool) = fun w p ->
  exist_prop w (if is_vattr p then attr_vattr p else p)

let (transducer: world -> (property -> iproperty option) -> filename -> itransducer) =
 fun w conv filename ->
  let transducers =
    let suffix =
      (* gnus compatibility trick *)
      if filename =~ "^[0-9]+$"
      then "mail"
      else filesuffix filename
    in

    set [transducer_system filename]
    $@$
    (find_trans w conv (Prop "transducer:") suffix
      +> map (function
      | (Left t) -> t
      | _ -> failwith "not a transducer plugin"
      )
    )
  in
  fun content ->
    transducers +> big_union (fun trans ->
      trans content
    )
    +> map_filter conv



let (transducer_sys: world -> (property -> iproperty option) -> filename -> itransducer) =
 fun w conv filename ->
  let transducers = set [transducer_system filename] in
  fun content ->
    transducers
    +> big_union (fun trans -> trans content)
    +> Common.map_filter conv



let (adv_transducer: world -> (property -> iproperty option) -> filename -> file -> adv_itransducer) =
 fun w conv filename file ->
  let transducers =
   set [adv_transducer_system conv filename file] $@$
    (find_trans w conv (Prop "adv_transducer:")
     ((filesuffix filename)
        (* make compatibility trick *)
       +> (fun s -> if filename = "Makefile" then "make" else s
       ))
     +> map (function
     | (Right t) -> t
     | _ -> failwith "not an advanced transducer plugin")
    )
  in
  fun parts ->
    transducers +> fold (fun props trans ->
      let newprops = trans parts in
      zip props newprops +> map (fun (s1, s2) -> (s1 $+$ s2))
     ) (parts +> map (fun part -> empty_list))




(*****************************************************************************)
(* The shell *)
(*****************************************************************************)
type path_element = Slash | Dot | DotDot | Element of formula
type path = path_element list

let (w: world ref) = ref (default_world ())

let (make_default_world: unit -> unit) = fun () ->
  w := default_world ()


let (pwd: unit -> formula) = fun () ->
  fst3 (top !w.pwd_history)


let lfs_check = ref true
let lfs_allow_cd_parts = ref true

type filename_or_id = (filename, idfile) either


(*---------------------------------------------------------------------------*)
(* todo?
 * IS_FORMULA?  ? is/of ?  ^ ? contain symbols, / ( ) & |, cf lexer
 *)
let valid_new_prop_name s =
  let is_not_basic_prop =
    (* normally useless to put them there, cos there is no way that
     * mkdir is called with such name, because cd will have intercept
     * such call before *)
    not (List.mem s [
      "true";
      "parts";
      "backdoor";

      ".ext"; ".int";
      ".strict";".relaxed";".best";".ca";
      ".classic";".parents";
      ".compat"; ".nocompat";
      ".nojump"; ".allowjump";

      "date:today";
      "date:yesterday";

      "_dircomputed";

      "."; "..";
    ])
  in
  let is_not_extension_prop =
    if is_vattr (Prop s)
    then
      not (List.mem (attr_vattr (Prop s) +> string_of_prop) [
        "inode:";
        "google:"; "glimpse:"; "agrep:"; "stree:"; "all:";
      ])
    else true
  in

  is_not_basic_prop
  && is_not_extension_prop
  (* this one is useful *)
  && (not (s =~ "noprop_.*"))


(* todo? check that not already a prop ? or a special prop ?
 * such as synchro: logic: ... Can contain special symbols ?
 * such as &!|
 *)
let valid_new_file_name s =
  not (List.mem s [
    "data"; (*  already used by real lfs *)
    "_dircomputed";
    "_backup";
    "RCS";
    "."; "..";
  ])
  && (not (s =~ ".*<[0-9]+>.*"))



let sanitize_lfs2 s =
  s +> Str.global_replace (Str.regexp "/") "_slash_"
    +> Str.global_replace (Str.regexp "\\.\\.") "_dotdot_"
    (* logic *)
    +> Str.global_replace (Str.regexp ")") "_lparen_"
    +> Str.global_replace (Str.regexp "(") "_rparen_"
    +> Str.global_replace (Str.regexp "&") "_and_"
    +> Str.global_replace (Str.regexp "|") "_or_"
    +> Str.global_replace (Str.regexp "!") "_not_"

    (* special file handling. cf also valid_new_file_name *)
    +> Str.global_replace (Str.regexp "\\<") "_inf_"
    +> Str.global_replace (Str.regexp "\\>") "_sup_"

    (* to avoid shell pb when use Common.command2 *)
    +> Str.global_replace (Str.regexp "\\$") "_dollar_"
    +> Str.global_replace (Str.regexp "#") "_sharp_"
    +> Str.global_replace (Str.regexp "%") "_percent_"
    +> Str.global_replace (Str.regexp "'") "_quote_"

let sanitize_lfs s =
  Common.profile_code "Lfs.sanitize" (fun () -> sanitize_lfs2 s)

let _ex1 = Common.example(sanitize_lfs "TODO<24>.txt" = "TODO_inf_24_sup_.txt")


(*---------------------------------------------------------------------------*)

(* emacs spirit *)
let hook_action_add_file =    ref [fun o  -> log3 "hook"]
let hook_action_del_file =    ref [fun o  -> log3 "hook"]

(* Diff with previous hooks ? more Real oriented *)
let hook_action_rm     = ref [fun o  -> log3 "hook"]
let hook_action_mkfile = ref [fun (o,name)  -> log3 "hook"]

let hook_action_change_file = ref [fun o  -> log3 "hook"]
(* todo? have a hook_change_filecontent and hook_change_file_props ? *)


(*---------------------------------------------------------------------------*)
(* Many commands mention valued attributes which do not have to be created
 * first. Kind of sugar. But they must nevertheless as with mkdir be added
 * to the property set.
 *)

(* fast_comm2, go further than fast_comm, dont create process per insertion,
 * create one process one and for all *)
let (_logics_server: ((property, logic) oassoc) ref) = ref
    (new oassocb [])
(* obsolete:
 *  fast_comm, dont create process per each comparaison, create only
 *  one per insertion (which involves many comparaison)
 *  => have to remember the process if dont want have "process leak"
 *       let _pending_logic_process = ref false
 *       let _in_logic = ref stdin
 *       let _out_logic = ref stdout
 *)


let (check_and_add_property: property -> iproperty option) = fun p ->
  log3 ("prop:" ^ (string_of_prop p));
  match () with
  | _ when not (valid_prop !w p) -> None
  | _ when     (exist_prop !w p) -> Some (!w.prop_iprop#assoc p)
  | _ when is_vattr p ->
      let attr = attr_vattr p in
      let iattr = !w.prop_iprop#assoc attr in
      (try ( Some (
       begin
        (* obsolete: let (|=) = find_alogic !w attr in ## if fast comm1 only *)

        (*  fast_comm2 *)
        let (|=) =
          if !_logics_server#haskey attr
          then !_logics_server#assoc attr
          else begin
            (* can return exn cos no logic (flat attr) *)
            let v = find_alogic !w attr in
            _logics_server := !_logics_server#add (attr, v);
            v
          end
        in

        (*  fast_logic *)
        (*  need know if formula  *)
        (try ignore(!w.cache_is_formula#assoc attr)
         with Not_found ->
          w := {!w with
            cache_is_formula =
              !w.cache_is_formula#add (attr, emptysb())
          });

        (if value_vattr p |= (Prop "IS_FORMULA?")
        then begin
          log2 ("found one");
          w := {!w with
            cache_is_formula = !w.cache_is_formula#apply attr
              (fun x -> x#add (value_vattr p))
          };
        end
        );
        (* let _ = Timing() in *)

        (*  FAST insert (et donc presque FAST LOGIC2 car avoid algo) *)
        (if (!w.cache_is_formula#assoc attr)#null
        then raise Not_found (* default logic is enough when have no formula *)
        (* obsolete:  if !_pending_logic_process then (ignore(Unix.close_process (!_in_logic, !_out_logic)); _pending_logic_process := false) in *)
        else ();
        );

        (* let _ = Timing() in *)
        let  (|=) =
          let cache = !w.cache_is_formula#assoc attr in
          (fun p1 p2 ->
            match (cache#mem p1, cache#mem p2) with
            | (false, false) -> p1 = p2
            | _ -> p1 |= p2
          )
        in
        (* let _ = Timing() in *)

       (* this one handle px prop in the full format (attr:xxx) not just xxx *)
        let (|=) = (fun p1 p2 ->
          match (is_attr p1, is_attr p2) with
          | (true, true) -> true           (* attr: |= attr: *)
          | (true, false) -> false         (* attr: |= attr:v is false *)
          | (false, true) -> true          (* attr:v |= attr: *)
          | (false, false) ->
                                           (* attr:x |= attr:y if x |= y *)
              (value_vattr p1) |= (value_vattr p2)
        )
        in
        let (graph, ip, inserted) =
          insert_property p (|=) (!w.graphp, !w.iprop_prop) iattr in

        (* let _ = Timing() in *)

        (* obsolete: cos of fast_comm
         *  if !_pending_logic_process then (ignore(Unix.close_process (!_in_logic, !_out_logic)); _pending_logic_process := false) in
         *)
        if not inserted
        (* subtil: may happen when 2 formulas have same semantic
         * (a AND b) et (b AND a), but store only one in cache
         *)
        then ip
        else begin
          (* let _ = Timing() in *)
          hook_action_add_prop +> Common.run_hooks_action p;
          w := {!w with
            graphp = graph;
            iprop_prop = !w.iprop_prop#add (ip, p);
            prop_iprop = !w.prop_iprop#add (p, ip);
          };
          w := {!w with
            (* It means that when cd parts, update extparts of course,
             * but also extfiles, normally with empty extension *)
            extfiles =
              !w.extfiles#add (ip,
                              (!w.graphp#successors ip)
                              +> big_union_ext (fun p -> !w.extfiles#assoc p))

          };
          (* bugfix: dont do '(if not (is_files_mode !w)) then'
           * cos if do a cd year:>xxx in lfs mode, then go in pof mode
           * must work too                                 ##OPT3 *)
          w := {!w with
            extparts =                                              (* OPT3 *)
              !w.extparts#add
                (ip,                                                (* OPT3 *)
                (!w.graphp#successors ip)                           (* OPT3 *)
                +> big_union_ext
                  (fun p -> try !w.extparts#assoc p
                            with Not_found -> empty_ext()))     (* OPT3 *)
          };                                                                      (* OPT3 *)
          ip
        end
      end
      )
      )
      with
      (*  when have no logic engine => flat properties *)
      | Not_found ->
        (* let _ = Timing() in *)
        let ip = new_iprop () in
        hook_action_add_prop +> Common.run_hooks_action p;
        w := {!w with
          graphp = (!w.graphp#add_node ip)#add_arc (iattr, ip);
          iprop_prop = !w.iprop_prop#add (ip, p);
          prop_iprop = !w.prop_iprop#add (p, ip);
        };
        w := {!w with extfiles = !w.extfiles#add (ip, empty_ext())};

        if lfs_mode !w = Parts
        then
          w := {!w with extparts = !w.extparts#add (ip, empty_ext())}; (* subtil: DOC *) (* OPT3 *)
        (* let _ = Timing() in *)
        Some ip
      | Timeout -> raise Timeout
      | e ->
          log ("pb with check, maybe a plugin die, exn = " ^
                  Printexc.to_string e);
         _logics_server := !_logics_server#delkey attr;
         None
      )
  | _ -> raise Impossible (* valid_prop and haskey => cant be here *)


let _ = _check_and_add := (fun (Prop s) -> check_and_add_property (Prop s))

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* cd,ls *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (ls2: unit -> ((property * int) set * idfile set)) = fun () ->
  let pwd = pwd () in
  let ctx = context !w in
  let (f, whichmode, options) = top !w.pwd_history in

  (* oldsimple:
   *  let dirs = dirs pwd mode ctx in
   *  (dirs#tolist,
   *   if is_files_mode !w
   *   then (objects pwd ctx dirs)#tolist
   *   else ...
   *)

  let ois = ext f ctx in

  let mode' = if options.ls_mode = Best then Relaxed else options.ls_mode in

  (* estet: cant inline in code after cos directories must be bound cos used by objects() *)
  let (directories, diropt2) = (* diropt2 is here for special case of CA computation, where want return as dir x&y&z fake property, but must also give original directories too, for objects computation *)
    match () with
    | _ when options.ls_mode = Best &&
              ( ((lfs_mode !w = Files) && ois#length <= 20) || (* CONFIG *)
                ((lfs_mode !w = Parts))
              ) ->
       let directories' = dirs pwd {options with ls_mode = Strict} ctx in
       if directories'#length <= 20 (* CONFIG *)
       then directories', None
       else (dirs pwd {options with ls_mode = mode'} ctx), None

    | _ when options.ls_mode = CA ->
        let directories' = (dirs pwd {options with ls_mode = Strict} ctx) in
        let candidates = directories'#tolist +> sort (fun (p1,i1) (p2,i2) -> - (compare i1 i2)) in
        let candidates = candidates +> map (fun (p,i) -> (p, i,
             (try (ctx.extensions#assoc (ctx.conv_prop#assoc p)) with Not_found -> empty_ext ()) $**$ ois)) in

        (* when .ca, try now also to gather in x&y&z *)
        (* for the moment do it tricky/ugly way by returning in ls in property in fact a formula, *)
        (*  but objects assume that there is only Single property => ls internally compute 2 directories *)
        (*  one return, and one pass to objects for this special case *)
        (*  less: if attr valué ? need put () around ? *)

        let rec aux acc = function
          | [] -> acc
          | ((p1,i1,ext1)::xs) as all ->
              (* old: aux  *)
              (* old:    (acc#add (p1,i1)) *)
              (* old:    (xs +> filter (fun (p2,i2,ext2) -> i1 = i2 || not (ext2 $<<=$ ext1))) *)
              (*  want agglomerate some property together, when they are equivalent *)
               let (acc1, acc2) = acc in
               let (same, xs) =
                 (* wrong: span (fun ((p1, i1, ext1), (p2, i2, ext2)) ->  ... *)
                 (* note: this is not a span, cos can have a list of dir with same sizeext,  *)
                 (*  but even if not first one is equal, maybe the second one will be => cant stop *)
                 (*  at first fail to equal, must continue *)
                 let rec spanlike = function
                   | []    -> ([], [])
                   | ((p2, i2, ext2) as x)::xs ->
                       if i1 = i2 && ext1 $==$ ext2 then
	                 let (l1, l2) = spanlike xs in
	                 (x::l1, l2)
                       else
                         if i1 = i2
                         then
	                   let (l1, l2) = spanlike xs in
	                   (l1, x::l2)
                         else ([], x::xs)
                 in
                 spanlike all
               in
               (* ugly: kind of fake Prop, cleaner would be that ls does not return a list of prop but a list of formula *)
               let bigfakeprop = same +> map (fun (Prop s, _, _) -> s) +> join "&" +> (fun s -> Prop s) in


               aux
                  (acc1#add (p1, i1),
                   acc2#add (bigfakeprop, i1)
                   )
                  (xs +> filter (fun (p2,i2,ext2) ->
                        (*opti*) i1 = i2 ||
                        not (ext2 $<<=$ ext1)))

        in
        aux (emptysb(), emptysb()) candidates +> (fun (acc1, acc2) -> (acc1, Some acc2))

  | _ when options.ls_mode = Int  ->
       if ois#length <> 1 || lfs_mode !w <> Files then raise Todo
       else
         let idfile = ois#getone in
         let file = !w.files#assoc idfile in
         let extr = file.extrinsic in
         let extr2 = extr +> List.map (fun ip -> let p = ctx.conv_iprop#assoc ip in p, 0) in
         ((emptysb())#fromlist extr2,
          None)

  (*  the general (simple) case *)
  | _ -> (dirs pwd {options with ls_mode = mode'} ctx), None
  in

  (directories#tolist +> (fun xs -> match diropt2 with None -> xs | Some dir2 -> dir2#tolist)  ,

   match lfs_mode !w with
   | Files ->
       if options.ls_mode = Best && ois#length <= 10  (* CONFIG *)
       then ois#tolist
       else (objects pwd ctx directories)#tolist
   | Parts ->
      (* when one file, can have strange effect, dont see anymore the file cos no more, prop in this view => this special case *)
       if length !w.parts = 1
       then !w.parts +> big_union (fun (id, info) -> set [id])
       else ((ext pwd ctx)#fold (fun acc o -> acc#add (!w.partsfile#assoc o)) (emptysb()))#tolist
   )
let ls a =
  Common.profile_code "Lfs.ls" (fun () -> ls2 a)


let id_to_filename2 = fun id -> (!w.files#assoc id).filename
let id_to_filename a =
  Common.profile_code "Lfs.id_to_filename" (fun () -> id_to_filename2 a)

let ls_filenames()  =
  ls() +> snd +> List.map id_to_filename
let ls_id_of_name s =
  ls() +> snd +> List.find (fun id -> id_to_filename id = s)


(*---------------------------------------------------------------------------*)
let rec (cd2: path_element -> unit) = function
  | Slash ->  w := {!w with pwd_history = push ((Single root), Files, default_mode()) empty_list}
  | Dot   -> ()
  | DotDot -> w := {!w with pwd_history = pop !w.pwd_history}

  | Element (Single (Prop "prop_to_test_timeout")) ->
      while true do
        (* have to do something otherwise the GC will not be triggered
         * and timeout signal detection are triggered only when the GC
         * is launched
         *)
        let _x = List.map (fun x -> x + 1) [1;3;4] in
        ()
      done

  | Element (Single (Prop "backdoor")) -> cd Slash (* :) *)

  (* relation *)
  | Element (Single (Prop x)) when x =~ "\\(.*\\):\\([io][sf]\\)=>=" ->
      let (rel, kind) = matched2 x in
      assert (kind = "is" || kind = "of");
      ls() +> (fun (dirs, files) ->
        let newquery =
          dirs +> fold_left (fun a ((Prop s),_) ->
            if s =~ (rel ^ ":" ^ kind ^ "=i__\\([0-9]+\\)")
            then
              let os = matched1 s in
              let p = (Prop ("inode:" ^ os)) in
              (Or ((Single p, a)))
            else a
                            ) (Not (Single (Prop "true")))
        in
        w := {!w with pwd_history = push (newquery, Files, default_mode()) empty_list}
        )



  | Element (Single (Prop "parts")) ->
     if !lfs_allow_cd_parts then begin

     assert (lfs_mode !w = Files);
     let files_here = (ext (pwd ()) (context !w))#tolist in
     if files_here $=$ keys !w.parts
     then
       w := {!w with  pwd_history =
           push ((Single root), Parts, default_mode()) !w.pwd_history;
       }
     else begin
       (* in fact a fold insert_assoc *)
       let partsinfo = files_here +> map (fun idfile ->
         let file = !w.files#assoc idfile in
         log2 ("indexing:" ^ (i_to_s idfile));
         (idfile, create_parts idfile file
           (adv_transducer !w check_and_add_property file.filename file)
           ((fun () -> !w.prop_iprop), (fun () -> !w.iprop_prop)))
       ) in
       w := {!w with
         pwd_history = push ((Single root), Parts, default_mode()) !w.pwd_history;
         parts = partsinfo;
       };
       w := {!w with
         partsfile = !w.parts +> fold (fun acc (id, info) ->
           info.parts_info#fold (fun acc (idp, p) -> acc#add (idp, id)) acc
         ) (emptyab())
       };

       (*  need that the o in parts_info are ordered (or often ordered),
        * otherwise cache miss (and so slower) when #add with seti *)
       let (mino, maxo) =
         !w.parts +> fold (fun acc (id, info) ->
           info.parts_info#fold (fun (mino, maxo) (idp, p) -> (min idp mino, max idp maxo)) acc
         ) (max_int, 0) in
       log2 (sprintf "min = %d, max = %d" mino maxo);

       (* let _ = Timing() in *)
       let extparts = (* OPT3 *)

(*          !w.parts +> fold (fun acc (_idf, info) ->                                                            ##OPT3 *)
(*            info.parts_info#fold (fun extparts (o, p) ->                                                       ##OPT3 *)
          enum mino maxo +> fold (fun extparts o ->
            let idf = !w.partsfile#assoc o in
            let info = !w.parts +> assoc idf in
            let p = info.parts_info#assoc o in
             upward_props p.pdescription !w.graphp                                                              (* OPT3 *)
               +> fold (fun extparts p ->            (* apply_assoc p (fun x -> x $+$ set [o]) extp *)          (* OPT3 *)
                 (* ESTET? *)                                                                                   (* OPT3 *)
                   let _ = count1 () in
                 match (try Some (extparts#assoc p) with Not_found -> None) with (* apply with assoc default *) (* OPT3 *)
                 | None   -> let _ = count3 () in extparts#add (p,   (empty_ext())#add o  )                     (* OPT3 *)
                 | Some x -> let _ = count4 () in extparts#replkey (p, x#add o)                                 (* OPT3 *)
                        ) extparts                                                                              (* OPT3 *)
(*             ) acc                                                                                             ##OPT3 *)
         ) (!empty_extparts()) in                                                                               (* OPT3 *)
       (* let _ = Timing() in                                                                                      (* OPT3 *) *)
       w := {!w with extparts = extparts};                                                                      (* OPT3 *)
       ()
     end
     end


  | Element (Single (Prop x)) when member x
        [".ext"; ".strict";".relaxed";".best";
         ".classic";".parents";".ca";".int"] ->
      w := {!w with pwd_history =
              let (f, whichmode, options) = top !w.pwd_history in
              push (f, whichmode,
                   {options with ls_mode =
                       (assoc x
                           [ (".ext",     Ext);
                             (".int",     Int);
                             (".strict",  Strict);
                             (".relaxed", Relaxed);
                             (".ca",      CA);
                             (".best",    Best);
                             (".classic", Classic);
                             (".parents", Parents)
                           ])}) !w.pwd_history;
      };
  | Element (Single (Prop x)) when member x [".compat"; ".nocompat"] ->
      w := {!w with pwd_history =
          let (f, whichmode, options) = top !w.pwd_history in
          push (f, whichmode, {options with mkdir_mode =
              (assoc x [".compat", Compat; ".nocompat", Normal])})
            !w.pwd_history;
      };

  | Element (Single (Prop x)) when member x [".nojump"; ".allowjump"] ->
      w := {!w with pwd_history =
          let (f, whichmode, options) = top !w.pwd_history in
          push (f, whichmode, {options with cd_mode =
              (assoc x [".nojump", NoJump; ".allowjump", AllowJump])})
            !w.pwd_history;
      };

  | Element (Single (Prop x)) when x =~ "\\(.*\\)\\^" -> let prop = matched1 x in
      (cd (Element (Single (Prop prop))); (* choice: pourrait aussi decider que cd a^ ne selectionne pas le a, juste vue *)
       w := {!w with pwd_history =
              let (f, whichmode, options) = top !w.pwd_history in
              push (f, whichmode, {options with view_mode = SingleV (Prop prop)}) (pop !w.pwd_history);
            };)
  | Element (Or (Single (Prop x1), Single (Prop x2))) when x1 =~ "\\(.*\\)\\^" && x2 =~ "\\(.*\\)\\^" ->
      let _ = x1 =~ "\\(.*\\)\\^" in let p1 = matched1 x1 in
      let _ = x2 =~ "\\(.*\\)\\^" in let p2 = matched1 x2 in
       w := {!w with pwd_history =
              let (f, whichmode, options) = top !w.pwd_history in
              push (f, whichmode, {options with view_mode = OrV (Prop p1, Prop p2)}) (pop !w.pwd_history);
            }
  | Element (Not (Single (Prop x))) when x =~ "\\(.*\\)\\^" -> let prop = matched1 x in
       w := {!w with pwd_history =
              let (f, whichmode, options) = top !w.pwd_history in
              push (f, whichmode, {options with view_mode = NotV (Prop prop)}) (pop !w.pwd_history);
            }


  | Element (Single (Prop ".specialv")) ->
       w := {!w with pwd_history =
              let (f, whichmode, options) = top !w.pwd_history in
              push (f, whichmode, {options with view_mode = SpecialV}) (pop !w.pwd_history);
            }


  | Element f ->
      if (thd3 (top !w.pwd_history)).cd_mode = NoJump
      then raise Not_found;

      (properties_of_formula f) +> iter (fun p ->
        if not (is_special_prop p) && check_and_add_property p = None
        then raise Not_found
      );
      w := {!w with pwd_history =
             let (oldf, whichmode, mode) = top !w.pwd_history in
             let newf =
               (* old: (if oldf = Single root then f else And (f, oldf)) *)
               (* new: to fix the "minp pb". *)
               (* note: suppose that fold the and right (a/b/c => And (c, And (b, a))) last in first out *)
               (match (oldf, f) with
               | (Single x, _) when x = root -> f
               | (Single oldp, Single newp) when not (is_special_prop oldp) && not (is_special_prop newp) ->
                   let ioldp = !w.prop_iprop#assoc oldp in
                   let inewp = !w.prop_iprop#assoc newp in
                   let parents = !w.graphp#predecessors inewp in
                   if ioldp $??$ parents then Single newp else And (f, oldf)
               | (And (Single oldp, x2), Single newp) when not (is_special_prop oldp) && not (is_special_prop newp) ->
                   let ioldp = !w.prop_iprop#assoc oldp in
                   let inewp = !w.prop_iprop#assoc newp in
                   let parents = !w.graphp#predecessors inewp in
                   if ioldp $??$ parents then And (Single newp, x2) else And (f, oldf)
               | (oldf, newf) -> And (newf, oldf)
               ) in
             push (newf, whichmode, mode) !w.pwd_history;
           };

and cd a =
  Common.profile_code "Lfs.cd" (fun () -> cd2 a)

let (dopath: path -> (unit -> 'a) -> 'a) = fun path op ->
  let old_pwd = !w.pwd_history in
  path +> iter (fun p -> cd p);
  let x = op () in
  w := {!w with pwd_history = old_pwd};
  x


(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* mkdir,mkfile *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (mkdir: string -> unit) = fun name ->
  let p = Prop name in
  let pwd = pwd () in
  assert (not (exist_prop !w p));
  assert (valid_new_prop_name name);
  (* robust: check simple atom ?, check no special sym *)
  (* extend: or filter not/or *)
  assert (is_conjunction pwd);

  let ps = properties_of_formula pwd +> List.map !w.prop_iprop#assoc  in
  let ps =
    if (Common.thd3 (top !w.pwd_history)).mkdir_mode = Compat
    then [(Prop "props-misc")] +> List.map !w.prop_iprop#assoc
    else ps
  in
  let ip = new_iprop () in

  hook_action_add_prop +> Common.run_hooks_action p;
  w := {!w with
    graphp = (!w.graphp#add_node ip) +> (fun g ->
      ps +> fold (fun g prop -> g#add_arc (prop, ip)) g
    );
    iprop_prop = !w.iprop_prop#add (ip, p);
    prop_iprop = !w.prop_iprop#add (p, ip);
    extfiles = !w.extfiles#add (ip, empty_ext());
  }

(*---------------------------------------------------------------------------*)
(* note: this function does not call the file transducer, this is done in
 * write now, as usually in real mode mkfile get an empty file content.
 *)
let (mkfile2: filename -> filecontent -> plugin option -> idfile) =
 fun name content plugin ->
  let pwd = pwd () in

  Common.profile_code "Lfs.mkfile(checks)" (fun () ->

  (* work too in Parts, but zarb. Better assert cos was certainly a mistake
   * from user. Or if allow should make it available from this
   * cd parts => re create_parts)
   *)
  assert (lfs_mode !w = Files);

  (* todo? check that a file with same prop does not exist ? name not in P ?
   * Mais lourd, costly , and compare intrinsic too ?
   *
   * note: with the inode: stuff, now it's always unique.
   * robust: he may not be here because of intrinsic prop or zarb ls
   * => have to do some check
   *)
  assert (valid_new_file_name name);
  (* old: Assert (is_conjunction pwd); (* extend: or filter not/or *) *)

  (* needed only for core lfs, otherwise linux will not call us I think *)
  if !lfs_check
  then assert (not (name $?$ ls_filenames() ));
  );

  let o = new_object () in
  let fcontent = !core_set_fcontent__fst (Core content, o) in

  Common.profile_code "Lfs.mkfile(hooks)" (fun () ->
  hook_action_add_file +> Common.run_hooks_action o;
  hook_action_mkfile +> Common.run_hooks_action (o, name);
  );

  (* opti: call only system transducer, cos useless call transducer plugin
   * with empty content :) (normally).
   *
   * note: Would be called in release (write2) de toute facon ?
   * No if create empty file will not be called in release
   * => at least we want file have system properties.
   *
   * note: so if use semi_real_mode, then calling mkfile is not enough!
   *)
  let intr =
    Common.profile_code "Lfs.mkfile(2)" (fun () ->
    if !_realfs
    then fcontent +> (transducer_sys !w check_and_add_property name)
    else fcontent +> (transducer     !w check_and_add_property name)
    )
  in
  let extr = properties_conj_of_formula pwd +> map !w.prop_iprop#assoc in

  (* todo: handle taxo, could do a min|= but costly
   *  => may have to do multiple mv to suppr prop (tant pis)
   *)

  let file = {
    filename  = name;
    fcontent  = fcontent;
    extrinsic = extr;
    intrinsic = intr;
  } in

  Common.profile_code "Lfs.mkfile(3)" (fun () ->
  w := {!w with
    files = !w.files#add (o, file);
    plugins =
      (match plugin with
      | Some x -> insert_assoc (o, x) !w.plugins
      | None -> !w.plugins
      );
    extfiles =
      !w.graphp
        +> upward_props  (file.extrinsic $+$ file.intrinsic)
        +> fold (fun extfile p ->
          extfile#apply p (fun x -> x#add o)
        ) !w.extfiles
  };
  );
  o

let mkfile a b c =
  Common.profile_code "Lfs.mkfile" (fun () -> mkfile2 a b c)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* +rm,mv+ *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* the possibility to pass directly the idfile allow to handle ambiguous
 * file (eg rm foo.<xx>), and maybe to optimize a little.
 *)
let (rm: filename_or_id -> idfile) = fun fid ->
  assert (lfs_mode !w = Files); (* extend: could *)

  let o = match fid with
  | Left s -> ls_id_of_name s
  | Right o -> o
  in
  let file = !w.files#assoc o in
  hook_action_del_file +> Common.run_hooks_action o;
  hook_action_rm +> Common.run_hooks_action o;
  w:= {!w with
    files   = !w.files#delkey o ;
    plugins = del_assoc o !w.plugins;
    extfiles =
      (* bugfix: subtil: need add iroot, cos maybe after many rmdir or mv,
       * the file has no more properties, but we must still when we rm it
       * to erase it from the extension of iroot at least.
       * Thanks to rix beck for pointing out this bug.
       *)
      !w.graphp
      +> upward_props (file.extrinsic $+$ file.intrinsic $+$ set [iroot])
      +> fold (fun extfile p ->
          extfile#apply p (fun x -> x#del o)
      ) !w.extfiles
  };
  o

(*---------------------------------------------------------------------------*)
(* possibility to pass directly the idfile allow to handle ambiguous
 * file (eg mv foo.<xx> bar/) *)
let rec (mv: filename_or_id -> path -> filename -> unit) =
 fun fid newpath newname ->

  assert (lfs_mode !w = Files);
  assert (valid_new_file_name newname);

  let (o, oldname) =
    match fid with
    | Left oldname -> ls_id_of_name oldname, oldname
    | Right o ->      o,                     (!w.files#assoc o).filename
  in

  let file = !w.files#assoc o in

  let oldpwd = pwd() in
  let oldprops = properties_conj_of_formula oldpwd +> map !w.prop_iprop#assoc
  in

  dopath newpath (fun () ->

    (* todo? in fact should erase content but not sure can get called in
     * that case, cos linux must transform this in a cp and rm of oldfile
     * without asking us *)
    assert (lfs_mode !w = Files);

    (* no more true, because now want mv ambiguous filename, such as
     * toto<45>.c.
     * old: assert (not (newname $?$ ls_filenames() ));  *)

    let newpwd = pwd() in
    let newprops = properties_conj_of_formula newpwd +> map !w.prop_iprop#assoc
    in

    (* todo? handle strange case such as removing/adding a general prop
     * wheras file have specific prop (have to be precise) *)
    let toadd = (newprops $-$ oldprops) $-$ (file.extrinsic) in
    (* avoid suppr not existing *)
    let todel = (oldprops $-$ newprops) $*$ (file.extrinsic) in

    let newfile = {file with
                    extrinsic = (file.extrinsic $-$ todel) $+$ toadd;
                    filename = newname;
                  } in

    (* robust: if mv a plugins, must flatten the old props ? or let them
     * as is in the cache ? *)


    (* y'a juste a faire attention aux todel, c eux les critiques.
     * indeed we must not del o from extension of root just because
     * we del a prop from the descr of o,  cos the other prop are also
     * children of root.
     *
     * first there is a pb when del prop from descr, without adding new one.
     * so should re-add prop from newdescr,  or more efficient (?) compute
     * just needed
     *
     *
     * but if del a general prop from intrinsic ? so should re-add also
     * intrinsic ? no cos $*$ (file.extrinsic) for todel ?
     * but if in extrinsic have general prop of an intrinsic (duplication),
     * then no more invariant
     *  => either ensure that never have a duplication between extr and
     *  intr (in mkfile, mv, ...) but complex (cos of prop general/specific)
     *  => either compute.
     *
     * (sinon faudrait faire un union des children)
     *)

    let todel =
      upward_props todel !w.graphp
      $-$
      upward_props (newfile.extrinsic $+$ newfile.intrinsic) !w.graphp in
    (* could avoid some mais bon. *)
    let toadd = upward_props toadd !w.graphp in


    w := {!w with files = !w.files#replkey (o, newfile)};
    w := {!w with extfiles = todel  +> fold (fun extf p ->
      extf#apply p (fun x -> x#del o)) !w.extfiles};
    w := {!w with extfiles = toadd  +> fold (fun extf p ->
      extf#apply p (fun x -> x#add o)) !w.extfiles};

    (* have to call transducer cos name:/ext: may have changed *)
    if newname <> oldname then transduce_file o;
  )


and transduce_file idfile =
  let file   = !w.files#assoc idfile in

  (* todo:   let fcontent = !core_set_fcontent__fst (Core newcontent, file.fcontent) in  *)
  (* todo: to make work in core mode *)

  (*  some similarities with mv,  cos kind of mv but for intrinsic  *)
  (*  (could simplify here cos all the prop are mentionned, mais bon) *)
  let newprops = file.fcontent +> (transducer !w check_and_add_property file.filename) in
  let oldprops = file.intrinsic in

  let toadd = (newprops $-$ oldprops) $-$ (file.intrinsic) in
  let todel = (oldprops $-$ newprops) $*$ (file.intrinsic) (* avoid suppr not existing *) in

  let newfile = {file with
    intrinsic = (file.intrinsic $-$ todel) $+$ toadd; (*  = newprops this time. *)
  } in

  (*  y'a juste a faire attention aux todel, c eux les critiques *)
  let todel =
    upward_props todel !w.graphp
    $-$
    upward_props (newfile.extrinsic $+$ newfile.intrinsic) !w.graphp in
  let toadd = upward_props toadd !w.graphp in (*  could avoid some mais bon. *)

  w := {!w with files = !w.files#replkey (idfile, newfile)};
  w := {!w with extfiles = todel +> fold (fun extf p ->
    extf#apply p (fun x -> x#del idfile)) !w.extfiles};
  w := {!w with extfiles = toadd +> fold (fun extf p ->
    extf#apply p (fun x -> x#add idfile)) !w.extfiles};
  ()

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* +rmdir,mvdir+ *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (rmdir: string -> unit) = fun name ->
  let p = Prop name in
  assert (exist_prop !w p);
  assert (lfs_mode !w = Files); (* extend?: could, but need update extparts too *)

  let ip = !w.prop_iprop#assoc p in
  let extprop = !w.extfiles#assoc ip in
  let parents =  (!w.graphp#predecessors ip)#tolist in
  let children = !w.graphp#successors ip in

  if (thd3 (top !w.pwd_history)).mkdir_mode = Compat
  then failwith "rmdir pb: rmdir forbidden in compatibility mode";

  assert (children#null);
  (* extend: or adjust axioms/logic cos want be able to erase formula
   * (and adjust descr get more complicated) *)

  w:= {!w with
    graphp = (!w.graphp
               +> (fun g -> parents +> fold (fun g prop ->
                 g#del_arc (prop, ip)) g))
               #del_node ip;
    iprop_prop = !w.iprop_prop#delkey ip;
    prop_iprop = !w.prop_iprop#delkey p;
    extfiles = !w.extfiles#delkey ip;
    files = extprop#fold (fun files id ->
      let file = !w.files#assoc id in
      files#replkey (id, {file with
        (* simple if allow only delete prop that have no child,
         * forcement dans descr *)
        extrinsic = file.extrinsic $-$ set [ip];
        intrinsic = file.intrinsic $-$ set [ip];
      } )
    ) !w.files
  }

(*---------------------------------------------------------------------------*)
let (mvdir: string -> path -> string -> unit) = fun oldname newpath newname ->
  let oldp = Prop oldname in
  let newp = Prop newname in

  assert (valid_new_prop_name newname);

  (* todo: I put condition on files_mode,  cos dont want update extparts,
   * but could mv in parts after all, could even mvdir between world
   * cd /lfs; mv  function:c/  parts/, the pwd will shunte accordingly *)
  assert (lfs_mode !w = Files);
  assert (exist_prop !w oldp);
  if newp <> oldp
  then assert (not (exist_prop !w newp));

  let ip = !w.prop_iprop#assoc oldp in
  let oldpwd = pwd() in
  let oldprops = properties_of_formula oldpwd +> map !w.prop_iprop#assoc in

  dopath newpath (fun () ->

    let newpwd = pwd() in
    let newprops = properties_of_formula newpwd +> map !w.prop_iprop#assoc in

    assert (lfs_mode !w = Files);
    assert (is_conjunction oldpwd);
    assert (is_conjunction newpwd);

    let axioms = (!w.graphp#predecessors ip)#tolist in

    let toadd = (newprops $-$ oldprops) $-$ (axioms) in
    (* avoid suppr not existing *)
    let todel = (oldprops $-$ newprops) $*$ (axioms) in

    let newprops = upward_props toadd !w.graphp in
    let extprop = !w.extfiles#assoc ip in


    (* todo: allow del stuff  (and so adjust ext may be complex ? have do an union of child, fleche forte/faible pb ?) *)
    (* old: assert (null todel); *)
    (* todo: replace succressos by children, or opti by looking if ip is in ancestors of ip2 *)

    (* what is this check ?  if want to mv props-type/type: to props-system/type:
     * then do we have to remove the prop prop-system to the file that
     * had a type: ? maybe, but maybe they are in props-system because
     * of other props under props-system or maybe even props-system itself.
     * pb fleche forte/faible.
     *)
    assert( todel +> forall (fun ip ->
      toadd +> exists (fun ip2 -> ip2 $??$ !w.graphp#successors ip)));


    (* let _ = Timing() in *)
    if newp <> oldp then begin
        w := { !w with iprop_prop = !w.iprop_prop#delkey ip };
        w := { !w with prop_iprop = !w.prop_iprop#delkey oldp };
        w := { !w with iprop_prop = !w.iprop_prop#add (ip, newp) };
        w := { !w with prop_iprop = !w.prop_iprop#add (newp, ip) };
    end;
    (* let _ = Timing() in *)

    w := {!w with extfiles = newprops +> fold (fun extf p ->
      extf#apply p (fun x -> x $++$ extprop)) !w.extfiles};
    w := {!w with graphp = toadd +> fold (fun g prop ->
      g#add_arc (prop, ip)) !w.graphp};
    w := {!w with graphp = todel +> fold (fun g prop ->
      g#del_arc (prop, ip)) !w.graphp};

    (* robust: avoid cycle *)

    )

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* +read,write+ *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (read: filename -> filecontent) = fun name ->
  let idfile = ls_id_of_name name in
  let file = !w.files#assoc idfile in

  if lfs_mode !w = Files
  then !core_get_fcontent__id  file.fcontent
  else view (pwd ()) (context !w) (assoc idfile !w.parts) +> fst

(*---------------------------------------------------------------------------*)
(* The idfile is passed cos transducer subtilty. Before I have a 'write' and
 * 'write2' function (and so code duplication). The subtilty is that when we
 * create a file "toto" in a dir, then this same file may not be listed
 * in ls, and so the write will fail, and so the file will not be
 * transduced.
 *)
let (write: filename_or_id -> filecontent -> 'a) = fun fid newcontent ->
  let idfile =
    match fid with
    | Left s -> ls_id_of_name s
    | Right o -> o
  in
  let file   = !w.files#assoc idfile in

  match lfs_mode !w with
  | Files ->
      hook_action_change_file +> Common.run_hooks_action idfile;
      transduce_file idfile
  | Parts ->
      let info = assoc idfile !w.parts in
      let (oldcontent, marks) = view (pwd ()) (context !w) info in

      let (newinfo, toadd, todel) =
      try
        (*  CONFIG *)
        (* let _ = raise Here in  *)
            (* IFNOT OPT6 let _ = raise Here in  *)
        (* reindex_diff_view idfile (newcontent, oldcontent) (file, info, marks) (adv_transducer !w check_and_add_property file.filename file) ((fun () -> !w.prop_iprop), (fun () -> !w.iprop_prop)) *)
        reindex_diff_view2 idfile (newcontent, oldcontent) (file, info, marks)
          (adv_transducer !w check_and_add_property file.filename file)
          check_and_add_property
          ((fun () -> !w.prop_iprop), (fun () -> !w.iprop_prop))
      with Here ->
        (* XX if we are here cos exn in reindex_diff, be sure that have good
         * content for original file,
         * XX cos reindex_parts do a diff with original file
         * (todo: could also generate it back from parts_info, will have
         * less pb)
         * XX  and so avoid not sync between parts_info and content
         * TODOOOOOOOO
         * note: strange behaviour when not pure info ? info still valid
         * for update_view ?
         * yes cos dont del obj in parts_info, just add one, and add only
         * at the end of reindex_diff so cant be here in that case
         *)

        let fullcontent = update_view newcontent info marks in
        (*  CONFIG *)
        reindex_parts idfile fullcontent (file, info)
          (adv_transducer !w check_and_add_property file.filename file)
          ((fun () -> !w.prop_iprop), (fun () -> !w.iprop_prop))
        (* reindex_parts2 idfile fullcontent (file, info) !w.prop_iprop !w.iprop_prop (adv_transducer !w check_and_add_property file.filename file) check_and_add_property *)
      in
      (* XXnote: the update to original must be done after !! cos reindex use
       * the original file on disque to get old version
       *  cant call update_view newcontent info marks,  cos info no more up
       * to date   this time (or need pure info data structure)
       * could reuse fullcontent computed above, we duplicate work,
       * but cleaner, et puis de toute facon reindex_parts on doit pas
       * l'utiliser. *)
      let finalcontent () =
        newinfo.line_to_part#fold (fun acc (i,o) ->
          (newinfo.parts_info#assoc o).pcontent :: acc) []
        +> rev +> unwords
      in

      let newfile = { file with
        fcontent = !core_update_fcontent__fst (finalcontent, idfile);
        (* todo: intrinsic = finalcontent +> (transducer !w file.filename); but cant do it if use opt reindex_diff, so have to delay it too with computation of fullcontent *)
      } in
      hook_action_change_file +> Common.run_hooks_action idfile;
      log2 (sprintf "add=%d, del=%d\n" (length toadd) (length todel));

      let extparts = !w.extparts in
      let extparts =                                                                    (* OPT3 *)
        todel +> fold (fun extparts o ->                                                (* OPT3 *)
        upward_props (info.parts_info#assoc o).pdescription !w.graphp                 (* OPT3 *)
          +> fold (fun extparts p ->
            extparts#apply p (fun x -> x#del o)) extparts    (* OPT3 *)
        ) extparts in                                                                   (* OPT3 *)

      (*  now can del obsolete obj, dont need anymore their description. *)
      let newinfo = {newinfo with parts_info = todel +> fold (fun acc o ->
        acc#delkey o) newinfo.parts_info} in

      (* better if parts_info have a iter that respect the order of
       * insertion, otherwise the insertion in seti will sux
       * todo: have good order ? cache miss in Seti.del ?
       *)
      (* let _ = Timing() in *)
      let extparts =                                                                                     (* OPT3 *)
        toadd +> fold (fun extparts o ->                                                                 (* OPT3 *)
          upward_props (newinfo.parts_info#assoc o).pdescription !w.graphp                               (* OPT3 *)
          +> fold (fun extparts p ->                                                                   (* OPT3 *)
               (* ESTET? *)                                                                            (* OPT3 *)
                match (try Some (extparts#assoc p) with Not_found -> None) with (* apply with assoc default *) (* OPT3 *)
                | None   -> extparts#add (p, (empty_ext())#add o)                                      (* OPT3 *)
                | Some x -> extparts#replkey (p, x#add o)                                              (* OPT3 *)
                      (* MEAN? extparts#apply p (fun x -> x#add o) *)                                  (* OPT3 *)
            ) extparts                                                                                 (* OPT3 *)
      ) extparts in                                                                                    (* OPT3   *)

     (* let _ = Timing() in *)
     w := {!w with
       files = !w.files#replkey (idfile, newfile);
       extparts = extparts;
       parts = !w.parts +> replace_assoc (idfile, newinfo);
       partsfile =
         toadd +> fold (fun acc o -> acc#add (o, idfile))
           (todel +> fold (fun acc o -> acc#delkey o)
               !w.partsfile);
     }



(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* relation. todo: allow path *)
let (ln: filename -> string -> filename -> unit) = fun a rel b ->
  let oa = ls_id_of_name a in
  let ob = ls_id_of_name b in
  match rel with
  | s when s =~ "<\\(.*\\):of>" ->
      let srel = matched1 s in
      (* ex: ln "alain" <father:of> "pad"
       *   ==>  give  alain the prop   father:of=i_pad
       *   ==>  give  pad   the prop   father:is=i_alain
       *
       * robust: have to be atomic, so if dont use transact, then
       * should check all conditions before proceedings.
       * with transact no pb.
       *)
      mv (Left a) [Element (And (pwd(), Single (Prop (srel ^ ":of=i__" ^ (i_to_s ob)))))] a;
      mv (Left b) [Element (And (pwd(), Single (Prop (srel ^ ":is=i__" ^ (i_to_s oa)))))] b;

  | _ -> failwith "not good relation format"

let relation_parse_path = ref (fun path -> failwith "not implemented")

let _ = add_hook hook_is_special_prop (fun p k ->
  match p with
  | (Prop x) when x =~ "\\(.*\\):of==" -> true
  | (Prop x) when x =~ "\\(.*\\):is==" -> true
  | (Prop x) when x =~ "\\(.*\\):of===" -> true
  | (Prop x) when x =~ "\\(.*\\):is===" -> true
  | (Prop x) when x =~ "\\(.*\\):of=>=" -> true
  | (Prop x) when x =~ "\\(.*\\):is=>=" -> true
  | p -> k p
  )

let _ = add_hook hook_compute_ext (fun (p,ctx) k  ->
  match p with
  | (Prop x) when x =~ "\\(.*\\):\\([io][sf]\\)===\\(.*\\)" ->
      let (rel, kind, q) = matched3 x in
      assert (kind = "is" || kind = "of");
       let path = !relation_parse_path q in
       (match path with
       | [Element f] ->
           let oi = (ext f ctx)#tolist in
           let disj = oi +> fold_left (fun a o ->
             let p = (Prop (rel ^ ":" ^ kind ^ "=i__" ^ (i_to_s o))) in
             (Or ((Single p, a))))
               (Not (Single (Prop "true"))) in
           ext disj ctx
       | _ -> failwith "bad syntax for formula in relation"
       )

  | (Prop x) when x =~ "\\(.*\\):\\([io][sf]\\)==\\(.*\\)" ->
      let (rel, kind, name) = matched3 x in
      assert (kind = "is" || kind = "of");
      let candidates = (ext (Single (Prop ("name:" ^ name))) ctx)#tolist in
      (match candidates with
      | [o] -> ext (Single (Prop (rel ^ ":" ^ kind ^ "=i__" ^ (i_to_s o)))) ctx
      | _   -> failwith "too much relation or not enough concerning this name, cant find the exact object"
      )
  | p -> k (p, ctx)
)


let ls () =
  ls () +> (fun (dirs, files) ->
   ( (dirs +> List.map (fun ((Prop s), i) ->
      match s with
      | s when s =~ "\\(.*\\):of=i__\\([0-9]+\\)" ->
          let (rel, os) = matched2 s in
          let file = !w.files#assoc (s_to_i os) in
          (Prop (rel ^ ":of==" ^ fileprefix file.filename), i)
      | s when s =~ "\\(.*\\):is=i__\\([0-9]+\\)" ->
          let (rel, os) = matched2 s in
          let file = !w.files#assoc (s_to_i os) in
          (Prop (rel ^ ":is==" ^ fileprefix file.filename), i)
      | _ -> ((Prop s), i)
   )),
   files)
  )


(*---------------------------------------------------------------------------*)

(* to incorporate existing ml plugins (used in demo) *)
let (wrap_transducer: ((filecontent -> property set) -> transducer)) =
 fun trans ->
  (function (Core content) -> trans content | _ -> raise Impossible)

(*
let (wrap_adv_transducer: adv_transducer -> adv_itransducer) = fun trans ->
  fun parts -> trans parts +> map (map_filter check_and_add_property)
*)

(* because of fast_logic *)
let (wrap_logic: logic -> (property -> bool) -> logic) =
 fun (|=) is_formula ->
   fun p1 p2 ->
     if p2 = (Prop "IS_FORMULA?")
     then is_formula p1
     else p1 |= p2
let default_is_formula = fun (Prop s) -> true

(* Spec/Impl compatibility, used in path for make demo.ml works *)
let wrap_dir = fun ((Prop s),_) -> s

let ls_bis ()        =
  ls() +> (fun (dirs, files) ->
    (dirs  +> List.map (fun p -> wrap_dir p ^ "/"))
      $+$
    (files +> map (fun id -> id_to_filename id))
  )

(*
# Local Variables:
# mode: tuareg
# End:
# eval: (message "Kikoo. You are currently watching a padator source. Enjoy")
*)
