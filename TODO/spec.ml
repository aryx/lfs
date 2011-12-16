open Common open Commonop

(*****************************************************************************)
(* Abstraction *)
(*****************************************************************************)
type property = Prop of string

type objet = {
    id:          identity;
    content:     content;
    description: property set;
  }
  and identity = int
  and content  = string

type context = {
    _P_: property set;
    _O_: objet set;
    _L_: logic;
  }
and logic    = (property -> property -> bool)  (*  mean: a |= b ? *)

(*---------------------------------------------------------------------------*)
type formula =
  | Single of property
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

let rec (ext: formula -> context -> objet set) = fun f ctx ->
  let (|=) = ctx._L_ in
  match f with
  | Single q ->
      ctx._O_ +> filter (fun o -> o.description +> exists (fun d -> d |= q))
  | And (f1, f2) -> (ext f1 ctx) $*$ (ext f2 ctx)
  | Or  (f1, f2) -> (ext f1 ctx) $+$ (ext f2 ctx)
  | Not f ->        (ctx._O_)    $-$ (ext f  ctx)

let (max_p: logic -> property set -> property set) = fun (|=) _P_ ->
  _P_ +> filter (fun p -> not (_P_ +> exists (fun p2 -> p2 <> p && p  |= p2)))

let (dirs: (formula -> context -> property set) ref ) = ref (fun f ctx ->
  max_p (ctx._L_) (ctx._P_ +> filter (fun p ->
    empty $<$ ext (And (f, Single p)) ctx &&
              ext (And (f, Single p)) ctx $<$ ext f ctx )))

let (objects: formula -> context -> objet set) = fun f ctx ->
  (ext f ctx) $-$ ((!dirs f ctx) +> big_union (fun p ->
    ext (And (f, Single p)) ctx))

(*****************************************************************************)
(* Concrete *)
(*****************************************************************************)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Concrete objects *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
type file = {
    filename: filename;
    extrinsic: property set;
    intrinsic: property set;
    fcontent: fakecontent;
  }
  and filename = string
  and filecontent = content
  (* Either the content in core lfs, or id from where we can get the path
   * to content in real lfs. So have in mind fakecontent = filecontent
   * when work in core lfs. If want print a beautiful code,  s/fake/file,
   * and s/core.*__(.+)/$1/.  Also some ugly stuff in context:  s/""/fcontent,
   * get rid of relaxed (and dirs as a ref), et de quelques comment
   *)
  and fakecontent = Core of filecontent | Real of identity

type idfile = identity

type transducer = (fakecontent -> property set)

(*---------------------------------------------------------------------------*)
type part = {
    fromfile: idfile;
    pnumber: int;
    pdescription: property set;
    pcontent: partcontent;
  }
  and partcontent = string

type idpart = identity

type adv_transducer = (partcontent list -> (property set) list)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Concrete context *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* the identities are used to make link between the real world and the
 * abstraction world (the context), and to avoid some redundancies in the
 * structure (for instance parts structure refer to files)
 *)
type world = {
    properties: property set;
    axioms: (property, property set) assoc; (* a |= b /\ c /\ d *)

    files: (idfile, file) assoc;
    parts: (idpart, part) assoc;
      filesparts:  idfile set;

    plugins: (idfile, plugin) assoc;

    pwd_history: (formula * whichmode) stack;
  }
  and whichmode = Files | Parts
  and plugin    =
    | Trans of transducer
    | AdvTrans of adv_transducer
    | Logic of logic

let root = Prop "true"

let default_world () = {
  properties = set [root];
  axioms     = empty;
  files = empty;
  parts = empty; filesparts = empty;
  plugins = empty;
  pwd_history = push ((Single root), Files) empty
}

let (is_files_mode: world -> bool) = fun w ->
  (w.pwd_history +> top +> snd) = Files
let (new_object: unit -> identity) = fun () ->
  counter ()

(*---------------------------------------------------------------------------*)
(* spec/impl compatibility *)
let wrap_dir =  fun (Prop s) -> s
let wrap_file = fun id w -> (w.files +> assoc id)
(* let wrap_adv_transducer f = f *)
let wrap_logic f x = f

(*---------------------------------------------------------------------------*)
(* core vs real lfs stuff *)
let _realfs = ref false

let (core_get_fcontent__id: (fakecontent -> filecontent) ref)   = ref
    (function (Core content) -> content | _ -> raise Impossible)
let (core_get_size_content__slength: (fakecontent -> string) ref) = ref
    (function (Core content) -> slength content  +> i_to_s | _ -> raise Impossible)
(*  normally called in mkfile and so have no content yet if realfs *)
let (core_set_fcontent__fst: ((fakecontent * identity) -> fakecontent) ref) = ref
    (function ((Core content), o) -> (Core content) | _ -> raise Impossible)
(*  this time have really a content *)
let core_update_content__fst = ref
    (function (content, o) -> (Core content))
  (*  in real, make io on disk with content, and return path of file with o *)

let (core_hook_logic: (idfile -> logic) ref) = ref
    (fun id ->      raise Not_found)
let (core_hook_transducer: (idfile -> property -> plugin) ref)   = ref
    (fun id prop -> raise Not_found)

let (wrap_transducer: ((filecontent -> property set) -> transducer)) = fun trans ->
  (function (Core content) -> trans content | _ -> raise Impossible)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* Concrete logic *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* valued attribute (vattr) are important in LFS, used to provide advanced
 * logic. Here we define some accessor to a vattr *)
let regexp_attr =  "^[a-zA-Z_]+:"

let (is_attr:  property -> bool) = fun (Prop s) -> s =~ (regexp_attr ^ "$")
let (is_vattr: property -> bool) = fun (Prop s) -> s =~ (regexp_attr ^ ".+$")

let (value_vattr: property -> property) = fun (Prop s) ->
  Prop (regexp_match s (regexp_attr ^ "\\(.+\\)$"))

let (attr_vattr: property -> property) = fun (Prop s) ->
  Prop (regexp_match s ("\\(" ^ regexp_attr ^ "\\)"))

let _ = example (is_attr  (Prop "toto:"))
let _ = example (is_vattr (Prop "toto:tata"))
let _ = example (value_vattr (Prop "toto:tata") = (Prop "tata"))
let _ = example (attr_vattr  (Prop "toto:tata") = (Prop "toto:"))

(*---------------------------------------------------------------------------*)
let (find_alogic: world -> property -> logic) = fun w ((Prop sprop) as attr) ->
  let plugins = w.files +> filter (fun (id, file) -> set [Prop ("logic:" ^ sprop)] $<=$ file.extrinsic) in
  assert (is_attr attr);
  assert (card_set plugins <= 1);
  if plugins = empty
  then (=) (* default logic when no plugins, flat attr *)
  else
    let (id, file) = top_set plugins in
    try (
    match (assoc id w.plugins) with
    |	Logic (|=) -> (|=)
    |	_ -> failwith "not a logic plugin"
     ) with Not_found -> !core_hook_logic id

(*---------------------------------------------------------------------------*)
let (valid_prop: world -> property -> bool) = fun w p ->
  (if is_vattr p then attr_vattr p else p) $?$ w.properties

(*****************************************************************************)
(* From concrete to abstract *)
(*****************************************************************************)
let (context: world -> context) = fun w ->
  {
    _P_ = w.properties;

    _L_ =
      (let rec (|=) = fun p1 p2 ->
	match (p1, p2) with
	| (x, Prop "true")   -> true  (* x |= true *)
	| (Prop "true", x)   -> false (* true |= x only when x = true *)
	| (x, y) when x = y  -> true  (* x |= x *)
     (* | _ -> false   ## far far faster if uncomment this,  enough with bibtex, cos no logic, no axiom, nearly just valued attribute *)

        (* attr:x |= attr:y if x |= y with dedicated logic engine *)
	| (ax, ay) when (is_vattr ax && is_vattr ay) &&
                        (attr_vattr ax = attr_vattr ay) ->
	    let (|=) = find_alogic w (attr_vattr ax) in
	    (value_vattr ax) |= (value_vattr ay)

        (* x |= z if there is an axiom x |= ... /\ y /\ ... and y |= z *)
	| (x, z) when x $?$ (keys w.axioms) ->
	    (assoc x w.axioms) +> exists (fun y -> y |= z)

        (* attr:x |= attr: is an axiom so attr:x |= y if attr: |= y *)
	| (ax, y) when is_vattr ax ->
	    (attr_vattr ax) |= y

	| _ -> failwith "internal error"
	in (|=)
      );

    _O_ =
      if is_files_mode w
      then w.files +> map (fun (idf, f) -> {
	id = idf;
        (*  f.fcontent; ## just to be beautiful cos dont used it *)
	content = "";
	description = f.intrinsic $+$ f.extrinsic;
      })
      else w.parts +> map (fun (idp, p) -> {
	id = idp;
	content = p.pcontent;
	description = p.pdescription;
      })

  }

(*****************************************************************************)
(* Transducers *)
(*****************************************************************************)
let transducer_system filename =
  (fun content -> set [
    Prop ("name:" ^ (fileprefix filename));
    Prop ("ext:" ^  (filesuffix filename));
    Prop ("size:" ^ (!core_get_size_content__slength content))
  ] )


let adv_transducer_system =
  (fun contents ->
    index_list contents +> map (fun (c, n) ->
      set [Prop ("part:" ^ (i_to_s n))]
    ))


(*---------------------------------------------------------------------------*)
let (find_trans: world -> property -> string -> plugin set) =
 fun w ((Prop sprop) as prop) suffix ->
  w.files
   +> filter (fun (id, file) ->
     set [Prop (sprop ^ suffix)] $<=$ file.extrinsic)
   +> map (fun (id, file) ->
     try assoc id w.plugins
     with Not_found -> !core_hook_transducer id prop
   )


let (transducer: world -> filename -> transducer) = fun w filename ->
  let transducers =
    set [transducer_system filename] $@$
    (find_trans w (Prop "transducer:") (filesuffix filename)
       +> map (function (Trans t) -> t | _ -> failwith "not a transducer plugin"))
  in
  fun content ->
    transducers +> big_union (fun trans -> trans content) +> filter (valid_prop w)

let (adv_transducer: world -> filename -> adv_transducer) = fun w filename ->
  let transducers =
    set [adv_transducer_system] $@$
    (find_trans w (Prop "adv_transducer:") (filesuffix filename)
     +> map (function (AdvTrans t) -> t | _ -> failwith "not an advanced transducer plugin"))
  in
  fun parts ->
    transducers +> fold (fun props trans ->
      let newprops = trans parts in
      zip props newprops +> map (fun (s1, s2) -> (s1 $+$ (s2 +> filter (valid_prop w))))
     ) (parts +> map (fun part -> empty))

(*---------------------------------------------------------------------------*)
let (index_parts: world -> idfile set -> (idpart, part) assoc) =
 fun w idfiles ->
  idfiles +> fold
    (fun acc id ->
      let file = assoc id w.files in
      let parts = lines_with_nl (!core_get_fcontent__id file.fcontent) in (* or tokens *)
      let part_prop = parts +> (adv_transducer w file.filename) +> zip parts +> index_list in
      part_prop +> fold (fun acc ((partcontent, props), partnumber) ->
	insert_assoc (new_object (),
		      { fromfile = id;
			pnumber = partnumber;
			pdescription = props;
			pcontent = partcontent;
		      }) acc
       ) acc
    ) empty

(*****************************************************************************)
(* The shell *)
(*****************************************************************************)
type path_element = Slash | Dot | DotDot | Element of formula
type path = path_element list

let (w: world ref) = ref default_world

let (pwd: unit -> formula) = fun () -> fst (top !w.pwd_history)

(*---------------------------------------------------------------------------*)
(* many commands mention valued attributes which do not have to be created
 * first, kind of sugar, but they must nevertheless as with mkdir be added
 * to the property set *)

let (check_and_add_properties: property set -> unit) = fun ps ->
  let _ = ps +> iter (fun p -> assert (valid_prop !w p)) in
  w := {!w with properties = !w.properties $+$ ps }

let rec (properties_of_formula: formula -> property set) = function
  | Single x -> set [x]
  | (And (f1, f2) | Or (f1, f2)) -> (properties_of_formula f1) $+$ (properties_of_formula f2)
  | Not f -> properties_of_formula f

let rec (is_conjunction: formula -> bool) = function
  | Single _ -> true
  | And (f1, f2) -> is_conjunction f1 && is_conjunction f2
  | ((Or  _)|(Not _)) -> false

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* cd/ls *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (cd: path_element -> unit) = function
  | Slash ->  w := {!w with pwd_history = push ((Single root), Files) empty}
  | Dot -> ()
  | DotDot -> w := {!w with pwd_history = pop !w.pwd_history}
  | Element (Single (Prop ".strict")) -> dirs := (fun f ctx ->
      max_p (ctx._L_) (ctx._P_ +> filter (fun p -> empty $<$ ext (And (f, Single p)) ctx && ext (And (f, Single p)) ctx $<$ ext f ctx )))
  | Element (Single (Prop ".relaxed")) -> dirs := (fun f ctx ->
      max_p (ctx._L_) (ctx._P_ +> filter
                    (fun p -> empty $<$ ext (And (f, Single p)) ctx &&
                              ext (And (f, Single p)) ctx $<=$ ext f ctx  (*  <= this time *)
                              && not (properties_of_formula f +> exists (fun pwd -> ctx._L_ pwd p)))))

  | Element (Single (Prop "parts")) ->
      assert (is_files_mode !w);
      let files_here = ext (pwd ()) (context !w)
	                +> map (fun obj -> obj.id) in
      if files_here $=$ !w.filesparts then (w := {!w with pwd_history = push ((Single root), Parts) !w.pwd_history;})
      else (*  mais bon gagne rien car fait presque rien dans index_parts kan spec *)
      (w := {!w with
	      pwd_history = push ((Single root), Parts) !w.pwd_history;
	      filesparts = files_here;
	      parts = index_parts !w files_here;
	    };
       check_and_add_properties (!w.parts +> big_union (fun (id, p) -> p.pdescription))
      )
  | Element f ->
      (w := {!w with pwd_history =
	     let (oldf, whichmode) = top !w.pwd_history in
	     push (And (f, oldf), whichmode) !w.pwd_history;
	   };
      check_and_add_properties (properties_of_formula f)
      )

let (dopath: path -> (unit -> 'a) -> 'a) = fun path op ->
  let old_pwd = !w.pwd_history in
  let _ = path +> iter (fun p -> cd p) in
  let x = op () in
  (w := {!w with pwd_history = old_pwd}; x)

(*---------------------------------------------------------------------------*)
let (ls: unit -> (property set * idfile set)) = fun () ->
  let pwd = pwd () in
  let ctx = context !w in
  (!dirs pwd ctx,
   if is_files_mode !w
   then objects pwd ctx +> map (fun o -> o.id)
   else (ext pwd ctx) +> big_union (fun o -> set [(assoc o.id !w.parts).fromfile])
  )

let ls_filenames()  =
  ls() +> snd +> map  (fun id -> (assoc id !w.files).filename)
let ls_id_of_name s =
  ls() +> snd +> find (fun id -> (assoc id !w.files).filename = s)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* mkdir/mkfile *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (mkdir: string -> unit) = fun name ->
  let pwd = pwd () in
  let ps = properties_of_formula pwd in

  assert (not ((Prop name) $?$ !w.properties));
  assert (not (name = "parts"))
  (* TODO check simple atom ?, check no special sym *);
  assert (is_conjunction pwd);

  w := {!w with
	 properties = set [Prop name] $+$ !w.properties;
	 axioms     = insert_assoc (Prop name, ps) !w.axioms;
       }

(*---------------------------------------------------------------------------*)
let (mkfile: filename -> filecontent -> plugin option -> idfile) = fun name content plugin ->
  let pwd = pwd () in
  let ps = properties_of_formula pwd in

  assert (is_files_mode !w); (* TODO? could *)
  assert (is_conjunction pwd);             (* TODO? or filter not/or *)
  assert (not (name $?$ ls_filenames() )); (* TODO need assert name not in P *)

  let o = new_object () in
  let fcontent = !core_set_fcontent__fst (Core content, o) in

  let file = { filename  = name;
	       fcontent  = fcontent;
	       extrinsic = ps;
	       intrinsic = fcontent +> (transducer !w name)
	     } in

  (w := {!w with files = insert_assoc (o, file) !w.files };
   (match plugin with
   | Some x    -> w := {!w with plugins = insert_assoc (o, x) !w.plugins }
   | _ -> ());
   check_and_add_properties file.intrinsic;
   o
  )

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* rm/mv *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (rm: filename -> unit) = fun s ->
  assert (is_files_mode !w); (* TODO? could *)
  let idfile = ls_id_of_name s in

  w:= {!w with
	files   = del_assoc idfile !w.files;
	plugins = del_assoc idfile !w.plugins;
      }

(*---------------------------------------------------------------------------*)
(* TODO minp pb: cd a/b/b2/c  mv ../../   ca va lui ajouter b, alors que
 * en fait. mais bon l'utilisateur a qu'a faire gaffe
 * (fleche forte/fleche faible) *)
let (mv: filename -> path -> filename -> unit) = fun oldname newpath newname ->
  assert (is_files_mode !w);
  let idfile = ls_id_of_name oldname in
  let file = assoc idfile !w.files in

  let oldpwd = pwd() in
  let oldprops = properties_of_formula oldpwd in

  dopath newpath (fun () ->
    let newpwd = pwd() in
    let newprops = properties_of_formula newpwd in

    assert(is_files_mode !w);
    assert(not (newname $?$ ls_filenames()));(* in fact should erase content *)
    assert(is_conjunction oldpwd) in
    assert(is_conjunction newpwd) in

    w:= {!w with files = !w.files +> replace_assoc
	    (idfile, {file with
		       extrinsic = (file.extrinsic $-$ oldprops) $+$ newprops;
		       filename = newname; (* TODO: call transducer_system *)
		     })}
  )

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* rmdir/mvdir *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let (rmdir: string -> unit) = fun s ->
  let p = Prop s in
  assert (p $?$ !w.properties);
  assert (is_files_mode !w); (* TODO? could *)
  (* TODO check have no child or adjust axioms *)

  w:= {!w with
	properties = !w.properties $-$ set [p];
	axioms = del_assoc p !w.axioms;
	files = !w.files +> map (fun (id, file) ->
	  (id, {file with
		 extrinsic = file.extrinsic $-$ set [p];
		 intrinsic = file.intrinsic $-$ set [p];
	       } ))
      }
(*---------------------------------------------------------------------------*)
let (mvdir: string -> path -> string -> unit) = fun oldname newpath newname ->
  let oldp = Prop oldname in
  let newp = Prop newname in

  assert (is_files_mode !w); (* TODO? could *)
  assert      (oldp $?$ !w.properties);
  if newp <> oldp
  then assert (not (newp $?$ !w.properties));

  let oldpwd = pwd() in
  let oldprops = properties_of_formula oldpwd in

  dopath newpath (fun () ->
    let newpwd = pwd() in
    let newprops = properties_of_formula newpwd in

    assert (is_files_mode !w);
    assert (is_conjunction oldpwd);
    assert (is_conjunction newpwd);

    let newaxioms = ((assoc oldp !w.axioms) $-$ oldprops) $+$ newprops in
    let (|=) = (context !w)._L_ in
    assert (not (newaxioms +> exists (fun p -> p |= oldp))); (* avoid cycle *)

    (* TODO change name *)
    w := {!w with axioms = !w.axioms +> replace_assoc (oldp, newaxioms) }
    )

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* read/write *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let mark_string___________ mark = (".........:" ^ (i_to_s mark))

let (view: formula -> idfile -> (filecontent * ((int, idpart list) assoc))) =
 fun pwd idfile ->
  let parts_pwd = ext pwd (context !w)
                    +> map (fun obj -> obj.id) in
  let all_parts = !w.parts +> filter (fun (id, part) -> part.fromfile = idfile)
                           +> sort (fun (id1, p1) (id2, p2) -> p1.pnumber <==> p2.pnumber) in

  let content = ref "" in
  let marks = ref empty in
  let mark = ref 1 in
  let pending = ref empty in

  all_parts +> iter (fun (id, part) ->
    if id $?$ parts_pwd
    then
      if !pending = empty
      then (content := !content ^ part.pcontent)
      else (content := !content ^ (mark_string___________ !mark) ^ "\n" ^ part.pcontent;
	    marks +!> insert_assoc (!mark, !pending);
	    mark := !mark + 1;
	    pending := empty;)
    else pending := !pending $@$ set [id]
   );
  if !pending = empty
  then (!content, !marks)
  else (!content ^ (mark_string___________ !mark) ^ "\n", insert_assoc (!mark, !pending) !marks)

(*---------------------------------------------------------------------------*)
let (read: filename -> filecontent) = fun name ->
  let idfile = ls_id_of_name name in
  let file = assoc idfile !w.files in
  if is_files_mode !w
  then !core_get_fcontent__id file.fcontent
  else view (pwd ()) idfile +> fst

(*---------------------------------------------------------------------------*)
let (write: filename -> filecontent -> unit) = fun name newcontent ->
  let idfile = ls_id_of_name name in
  let file   = assoc idfile !w.files in

  let finalcontent =
    if is_files_mode !w then newcontent
    else
      let marks = view (pwd ()) idfile +> snd in
      lines_with_nl newcontent +> map (fun s ->
	if s =~ ".*\\.\\.\\.\\.:"
	then let mark = regexp_match s ".*:\\([0-9]+\\)" +> s_to_i in
 	 assoc mark marks +> map (fun idpart -> (assoc idpart !w.parts).pcontent) +> unwords
	else s
	) +>  unwords in

  let fcontent = !core_update_content__fst (finalcontent, idfile) in
  let newfile = {file with
		  fcontent  = fcontent;
		  intrinsic = fcontent +> (transducer !w file.filename);
		} in

  w := {!w with files = !w.files +> replace_assoc (idfile, newfile)};
  check_and_add_properties newfile.intrinsic;

  if not (is_files_mode !w) then begin
    w := {!w with parts = index_parts !w !w.filesparts; };
    check_and_add_properties
      (!w.parts +> big_union (fun (id, p) -> p.pdescription)));
end
