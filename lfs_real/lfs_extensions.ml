open Ofullcommon

open Lfs

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* choices:  
 * - put it in cd (as was done first).
 *   but pb cos can not do !,| on a google:xxx,  
 * - put it in ext.
 *   but pb cos code get dirtier cos need access to w (soluce = hook techno) 
 *   pb too cos in cd we do a check_and_add_prop  on the props of the formula
 *   => encounter unknow google:xxx props  and even try to get their parents 
 *   pb too cos overhead cos of the regexp comparaison important ? 
 *   cos apply it each time want get ext of property (note that ext is not
 *   called in increment computation, cos directly do the assoc to get
 *   the extension of the possible increment) 
 * 
 * 
 * note: 
 * better to return an intension than an extension ? 
 * better to cache an extension, than a disjonction ?  
 * 
 * but pb with caching extension cos not always up to date with the prop. 
 * more robust cos rmdir is more rare than rm or mkfile, if use extension
 * then have to take care cos  could return a no more defined obj 
 * (and may make lfs in a zarb state) 
 * but slower :(  have to do a big union (and if refine the query then have
 * to do it here when compute ois_pwd) 
 * what are the consequences of mentionning in an extension an identifier no more used ?    
 * many assoc should fail (such as to get his filename, descr) 
 * 
 * todo: 
 *  have a dirty_lfs, cos may have to recompute normally 
 * (less grave when the cache return formula, cos  as agrep:xxx in 
 * lfs or pof mode is different (we filter some prop), the cache must
 * have for key a couple (lfsmode, formula) 
 *)


let (_cache_dynprop: ((string, Lfs.formula) oassoc) ref) = ref 
    (new oassocb []) 

let reset_cache () = 
  _cache_dynprop := (new oassocb [])

let _ = 
  begin
    Lfs.hook_action_add_prop +> add_hook_action (fun _ -> reset_cache ());
    Lfs.hook_action_add_file +> add_hook_action (fun _ -> reset_cache ());
    Lfs.hook_action_del_file +> add_hook_action (fun _ -> reset_cache ());
    Lfs.hook_action_change_file +> add_hook_action (fun _ -> reset_cache ());
  end

(*****************************************************************************)
(* inode:xxx special property *)
(*****************************************************************************)

let install_inode_extension () = 
  begin

    add_hook Lfs.hook_is_special_prop (fun p k -> 
      match p with 
      | (Prop x) when x =~ "inode:\\([0-9]+\\)" -> true
      | p -> k p
    );

    add_hook Lfs.hook_compute_ext (fun (p,ctx) k  -> 
      match p with
      | (Prop x) when x =~ "inode:\\([0-9]+\\)" -> let objs = matched1 x in
        let o = s_to_i objs in
        (empty_ext())#add o
      | p -> k (p, ctx)
    ); 
  end

(*****************************************************************************)
(* date:today special property *)
(*****************************************************************************)

let ext_time  time (p,ctx) k = 
     
  let s = "date:" ^ Common.string_of_unix_time_lfs time in
  
  (* note: calling directly ext does not work cos the property
     may not yet exist *)
  let _ = Lfs.check_and_add_property (Prop s) in 
  let ctx = Lfs.context !w in

  Lfs.ext (Single (Prop s)) ctx


let install_datetoday_extension () = 
  begin

    add_hook Lfs.hook_is_special_prop (fun p k -> 
      match p with 
      | (Prop x) when x = "date:today" -> true
      | (Prop x) when x = "date:yesterday" -> true
      | p -> k p
    );

    add_hook Lfs.hook_compute_ext (fun (p,ctx) k  -> 
      match p with
      | (Prop x) when x = "date:today" -> 
          let time = Unix.time () +> Unix.localtime in
          ext_time time (p,ctx) k
      | (Prop x) when x = "date:yesterday" -> 
          let time = Unix.time () +> Unix.localtime in
          let time' = {time with Unix.tm_mday = time.Unix.tm_mday - 1 } in
          let time = Unix.mktime time' +> snd in

          ext_time time (p,ctx) k

      | p -> k (p, ctx)
    );
  end


(*****************************************************************************)
(* glimpse:xxx,  agrep:xxx, stree:xxx *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* stree: et all:    suffix tree support part *)
(*---------------------------------------------------------------------------*)

let restricted_props = [
  "name:"; 
  "title:";
  "genre:"; "artist:";"album:";"playlist:"; "comment:";
  "domain:"; "author:";
  "paracontain:";
] 

let compute_suffix_tree () = 
  let strings = 
    if !Flag_lfs.stree_index_all_properties 
    then !w.prop_iprop#tolist +> List.rev_map (fun (Prop s, _)  -> s) 
    else 
      let parents = 
        restricted_props +> map (fun s -> !w.prop_iprop#assoc (Prop s)) in

      let children =
        parents +> fold_left (fun a p -> a $++$ (!w.graphp#successors p)) 
          (emptysb()) in
      children#tolist +> rev_map (fun ip -> string_of_prop (!w.iprop_prop#assoc ip))
  in
      
  let array = DynArray.of_list strings in
  
  (* lowercase, so stree: have implicetely a -i *)
  let strings = strings +> rev_map (fun s -> lowercase s) in
  let strings = strings +> fold (fun a s -> 
    (if s =~ "^[a-zA-Z_0-9]+:\\(.+\\)"
    then matched1 s
    else s
    )::a
  ) [] 
  in

  (* the number of fold has to be pair, so have same order than original *)
  let gst = Suffix_tree_ext.make strings in

  (* the suffix tree gst contain too internally an array of string, but
   *  it contains the lowercase array and without the attr, so need also
   *  keep the original array
   *)
  (gst, array)




let _suffix_info = Lazy.lazy_from_fun compute_suffix_tree

let grep_with_suffix prop =
  let (gst, array) = Lazy.force _suffix_info in
  (Suffix_tree_ext.exact_matches gst prop) +> map (fun (i,j) -> 
    DynArray.get array i
  )  

(* Could do as for glimpse, maintain a list of recently created prop, and then
 * use stree for old one and grep (or agrep) for recent one.
 * But adding in a stree is fast (compare to update glimpse), so do not
 * need the heavy tech used for glimpse
 *)


(*---------------------------------------------------------------------------*)
(* glimpse: support part *)
(*---------------------------------------------------------------------------*)

let (_new_files: objet oset ref) =  ref (emptysb())


(*---------------------------------------------------------------------------*)

(*  
 * glimpseindex note:
 *  - the -o or -b for glimpseindex => bigger index, faster search   
 *  - no need to use -b with our way to use glimpse 
 *   (cos use -l so dont need know what is the place of the word in the file)
 *  - the -f   for incremental, but handle well when files are deleted ? 
 *    I think that not that bad cos yes certainly in the index there will
 *    have some  no-more-valid pointers, but as glimpse actually then do
 *    a real search on the file, he will see that dont exist anymore and
 *   so using -f is slower but very very little slower 
 *  - the for -z, the order is important in .glimpse_filters => put
 *    the case of compressed file first 
 * 
 * Note que glimpseindex index pas forcement tous les fichiers texte. 
 * Si le fichier texte est trop petit, contient par exemple un seul mot, 
 * alors il l'indexe pas. Si veut indexer quand meme, il faudrait ajouter 
 * l'option -E
 * 
 * glimpse note:
 *  - the -N => far faster search (dont actually search the file 
 *   => when pdf/ps files 
 *   => no filtering done of them => far faster)
 *  - the -N fait pas un grep, donc si file deteled ou modified entre temps, 
 *    bah il le voit pas ca veut dire aussi que si y'a pas -N, et bien
 *    glimpse fait des grep si le fichier a ete modifié entre temps pour
 *    toujours filer quelque chose de valid (pas de false positive, mais 
 *    y'a quand meme peut etre des miss). Est ce qu'il utilise la date du
 *    fichier pour eviter de faire des grep inutile ?
 *  - the -N can actually return wrong result. cos a file may 
 *    contain "peter norvig"   
 *    => better to not use -N at first 
 *  - case  insensitive (-i) and match on complete words (-w) 
 *  - can add -w to glimpse,  but not always good idea (for instance 
 *    if file contain chazarain_j then dont work with -w) 
 *  - if use -z for glimpseindex, dont forget the -z too for glimpse 
 * 
 *)


let install_stree_glimpse_and_co_extension metapath use_glimpse = 
  begin
    hook_action_add_prop +> add_hook_action (fun (Prop p) -> 
      if Lazy.lazy_is_val _suffix_info 
      then 
        let (gst, array) = Lazy.force _suffix_info in
        Suffix_tree_ext.add 
          (* todo?: if not stree_index_all_properties, filter some prop p ?*)
          (p +> lowercase +> (fun s -> 
            if s =~ "^[a-zA-Z_0-9]+:\\(.+\\)"
            then matched1 s
            else s
          ))
          gst;
        DynArray.add array p
      else ()
    );

    hook_action_add_file +> add_hook_action (fun o -> 
      _new_files := !_new_files#add o
    );
    hook_action_del_file +> add_hook_action (fun o -> 
      _new_files := !_new_files#del o
    );
    hook_action_change_file +> add_hook_action (fun o -> 
      _new_files := !_new_files#add o
    );

    if use_glimpse then 
      Lfs_fuse.hook_action_umount +> add_hook_action (fun () -> 
        let notglimpsed = !_new_files#tolist in
        let filepaths = notglimpsed +> List.map (fun o -> 
          Lfs_real.obj_to_path metapath o
        ) in
        filepaths +> List.iter (fun path -> 
          log ("glimpseindex adding: "^ path);
          Common.command2 ("glimpseindex -o -H " ^ 
                              (metapath ^  "/glimpse/recent_files/") ^ 
                              " -a " ^ path)
        );
      );

    add_hook hook_is_special_prop (fun p k -> 
      match p with 
      | (Prop x) when x =~ "\\(google\\|glimpse\\|agrep\\|stree\\|all\\):\\(.*\\)" -> true
      | p -> k p
    );
    
    add_hook hook_compute_ext (fun (p,ctx) k  -> 
      match p with
      | (Prop x) when x =~ "\\(google\\|glimpse\\|agrep\\|stree\\|all\\):\\(.*\\)" -> 
          let (kind, prop) = matched2 x in
                                                                                      

          (* todo: detect too that if too many then fail, detect that if string
           * too short then fail (cos will have too many), ...) 
           *)
          
          (* if slength prop <= 2 then raise Not_found
           * else  *)
          if !_cache_dynprop#haskey x 
          then ext (!_cache_dynprop#assoc x) ctx
            (* robust: have to recheck that file exist always 
             * (cos glimpse is not always up to data) *)
          else

            let agrep_result = 
              if (kind = "agrep") 
              then
                (* opti: can suffix tree/array accelerate also an agrep query ? 
                 * at least to make a prepass to avoid iter over all props *)
                !w.prop_iprop#fold (fun a (Prop s, _) -> 
                  let n = Agrep.errors_substring_match 
                    (Agrep.pattern  ~transl:Agrep.Iso8859_15.case_and_accent_insensitive prop)  
                    ~numerrs: (if slength prop <= 6 then 0 else 1)  (* CONFIG *)
                    ~wholeword:false 
                    s 
                    ~pos:0 ~len:(slength s) 
                  in
                  if n = max_int
                  then a
                  else 
                    let _ = log (sprintf "agrep found: %s with %d errors(s)" s n) in
                    (Prop s)::a
                ) [] 
              else []
            in
            let stree_result = 
              if kind = "google" || kind = "stree" || kind = "all" 
              then
                let ys = grep_with_suffix prop in
                ys +> map (fun s -> Prop s)
                +> (fun xs -> ((emptysb())#fromlist xs)#tolist )
              else []
            in
            let glimpse_result = 

              if !_realfs && lfs_mode !w = Files && 
                (kind = "google" || kind = "glimpse" ) && 
                use_glimpse 
              then begin
                (* note: if call glimpseindex here (not advised)  need patched
                 * version of glimpse, otherwise glimpseindex call /bin/sync
                 * and so we have a deadlock (loop).
                 * 
                 * can add -f, I think that same behaviour
                 *   Common.command2("cd " ^ metapath ^ "; " ^ 
                 *                   " glimpseindex -o -z -H " ^ 
                 *                   metapath ^ " " ^ metapath ^ 
                 *                   "/files > /tmp/glimpse_lfs")
                 *)
                  let matched = 
                    Common.cmd_to_list ("glimpse -l -y -i -H " ^ 
                                        (metapath^"/glimpse/")^" "^ "\"" ^ prop ^ "\"")
                  in
                  matched +> List.iter (fun s -> log ("glimpse found: " ^ s));
                  
                  let matched2 = 
                    Common.cmd_to_list ("glimpse -l -y -i -H " ^ 
                                         (metapath^"/glimpse/recent_files") ^ " " ^ "\"" ^
                                         prop ^ "\"")
                  in
                  matched2 +> List.iter (fun s -> log ("glimpse recent found: " ^ s));
                  
                  let notglimpsed = !_new_files#tolist in
                  let filenames = notglimpsed +> List.map (fun o -> 
                    Lfs_real.obj_to_filename metapath o
                  ) in
                  let matched3 = 
                    Common.cmd_to_list ("grep -i -l -I " ^ "\"" ^ 
                                           prop ^ "\"" ^ " " ^ 
                                           (join " " filenames)) in
                  let matched3 = matched3 +> List.filter (fun s -> 
                    not ((s =~ "No such file or directory") || 
                         (s =~ "Binary file"))) 
                  in
                 
                  (matched ++ matched2 ++ matched3) +> fold (fun a s -> 
                    if s =~ ".*files/[0-9]+/\\([0-9]+\\)/" then
                      let numfile = matched1 s in
                      (Prop ("inode:" ^ numfile))::a
                    else a
                  ) []
              end
              else []
            in
            let result = agrep_result ++ stree_result ++ glimpse_result in

            let result = result +> filter (fun prop -> 
              if lfs_mode !w = Parts && 
                not (!w.extparts#haskey (!w.prop_iprop#assoc prop))
              then false 
              else true
            ) 
            in
            let disj = result +> fold_left (fun a p -> 
              (Or ((Single p, a)))) (Not (Single (Prop "true"))) 
            in
            
            let _ = _cache_dynprop := !_cache_dynprop#add (x, disj) in

            ext disj ctx

      | p -> k (p, ctx)
    );
  end


(*
  | Element (Single (Prop x)) when x =~ "\\*\\([^.].*\\)" -> 
      let prop = matched1 x in
      if prop =~ "\\(.+\\) \\(.+\\)" 
      then 
        let (x,y) = matched2 prop in
        if slength y >= 3 && slength x >= 3
        then (cd (Element (Single (Prop ("stree:" ^ x)))); 
              cd (Element (Single (Prop ("stree:" ^ y)))))
        else ((*cd (Element (Or ((Single (Prop ("stree:" ^ x))),*) cd (Element (Single (Prop ("stree:" ^ prop)))))
      else 
        (cd (Element (Single (Prop ("stree:" ^ prop)))))
*)

(*****************************************************************************)
