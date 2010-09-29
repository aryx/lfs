open Common

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* !!! if db are not oassoc_buffered, then many stuff slow down !!!
 * (especially context creation) 
 *)
let create_db name path_meta  env transact (fv, unv) = 
  let db = Bdb.Db.create env [] in 
  Bdb.Db.db_open db (transact()) 
    (path_meta ^ "/" ^ name ^ ".db4") 
    ("/" ^ name ^ ".db4") 
    Bdb.Db.DB_BTREE [Bdb.Db.DB_CREATE] 0;
  new Oassoc_buffer.oassoc_buffer !Flag_lfs.size_buffer_oassoc_buffer
    (new Oassocbdb.oassoc_btree db name transact fv unv),
  db



(* some fv, unv. cf oassocbdb.ml for comments *)
let setb_f = 
  (fun osetb -> osetb#tosetb), (fun setb -> new Osetb.osetb setb)
let seti_f =
  (fun oset -> oset#toseti), (fun set -> new Oseti.oseti set)
let id_f = Common.id, Common.id 


(*****************************************************************************)
(* oldsimple: persistence
    let (data:(int * Lfs.world)) = get_value (path_meta ^ "/ocaml_world") in
    begin 
     Lfs.w := snd data; 
     Common._counter := fst data
    end
   old: no persistence
     begin
      Lfs.w := Lfs.default_world; 
      Common._counter := 2; 
     end
*)


let launch_bdb path_meta use_transact = 
  (* rescue: 
   *  let init_not_reload = false in 
   *  Common._counter := 300000; 
   *  Common._counter2 := 300000 * 20;
   *  
   * pad: but less needed now, cos in check_world I set the counters to the
   * max object I found
   *
   * note: make sure that counter is regulary flush, and not created just
   * at umount time otherwise at next mount the counter will be reset as
   * iroot, and many other stuff  via adapt 
   *)
  let init_not_reload = 
    not (Sys.file_exists (path_meta ^ "/counter")) in
  let ifinit = fun f o -> 
    if init_not_reload then f o else o 
  in
  
  (* inode must be at least > 1, to avoid conflict with root inode = 1. 
   * As file id are inode id => same constraint. 
   * TODO freeobj 
   *)
  if init_not_reload 
  then Common._counter := 2 
  else begin 
    Common._counter  := Common.get_value (path_meta ^ "/counter");
    Common._counter2 := Common.get_value (path_meta ^ "/counter2");
  end;

  let env = Bdb.Env.create []  in
  
  Bdb.env_set_max_stuff env !Flag_lfs.bdb_size_tables;
  Bdb.Env.env_open env path_meta 
    [Bdb.Env.DB_CREATE;Bdb.Env.DB_INIT_LOG;Bdb.Env.DB_INIT_LOCK;
     Bdb.Env.DB_INIT_MPOOL;
     Bdb.Env.DB_INIT_TXN;Bdb.Env.DB_RECOVER;
    ] 
    (Int32.of_int 0);  
  
  (* transaction part 1 *)
  let t = ref        (Bdb.Txn.txn_begin env None []) in
  let newt () = t := (Bdb.Txn.txn_begin env None []) in
  let transact () = 
    if use_transact 
    then Some !t
    else None
  in
  let no_need_transact () = None in
  (* pad: before I had a transact1 and transact, why ? *)
  

  (* lfs db files *)
  let children, db1 = create_db "children" path_meta env transact setb_f in
  let parents, db2  = create_db "parents"  path_meta env transact setb_f in
  let iprop_prop, db3 = create_db "iprop_prop" path_meta env transact id_f in
  let prop_iprop, db4 = create_db "prop_iprop" path_meta env transact id_f in
  let cache_is_formula, db5 = 
    create_db "cache_is_formula" path_meta env transact setb_f in
  let files, db6 = create_db "filesdb" path_meta env transact  id_f in
  let extfiles, db7 = create_db "extfiles" path_meta env transact seti_f in
  
  (* pof db files *)
  let _count = ref 0 in
  
  if Flag_lfs.persistent_extparts 
  then 
    Lfs.empty_extparts := (fun () ->
      incr _count;
      create_db ("extparts" ^ i_to_s !_count) path_meta env no_need_transact 
        seti_f (* coupling: must be same seti than in empty_ext *)
        +> fst;
    );
  
  Lfs.empty_partsinfo := (fun () ->
    incr _count;
    (* note: I bufferise partsinfo just for the grouping of io, 
     * not for the multiple update benefit, cos it never happened for this 
     * assoc 
     *)
    create_db ("partsinfo"^ i_to_s !_count) path_meta env no_need_transact id_f
      +> fst;
  );
  Common.command2 ("rm -f " ^ path_meta ^ "/*extparts*");
  Common.command2 ("rm -f " ^ path_meta ^ "/*partsinfo*");
  (* the views *)
  Common.command2 ("rm -rf " ^ path_meta ^ "/files/2[0-9][0-9][0-9]/");
  
  
  let flush_db () =
    begin
      (* ugly: the misc_hook method is in fact flush, to use if use with 
       * caching (oassoc_buffer, cos table are buffered).
       *)
      children#misc_op_hook2;
      parents#misc_op_hook2;
      prop_iprop#misc_op_hook2;
      iprop_prop#misc_op_hook2;
      cache_is_formula#misc_op_hook2;
      files#misc_op_hook2;
      extfiles#misc_op_hook2;
      
      write_value !Common._counter (path_meta ^ "/counter");
      write_value !Common._counter2 (path_meta ^ "/counter2");
    end 
  in
  
  let default = Lfs.default_world () in
  
  Lfs.w := { default  with
    Lfs.graphp = (new Ograph2way.ograph2way children parents Lfs.emptysb)
      +> ifinit (fun o -> o#add_node Lfs.iroot);
    prop_iprop = prop_iprop
      +> ifinit (fun o -> o#add (Lfs.root, Lfs.iroot));
    iprop_prop = iprop_prop
      +> ifinit (fun o -> o#add (Lfs.iroot, Lfs.root));
    cache_is_formula = cache_is_formula;

    files = files;
    extfiles = extfiles
      +> ifinit (fun o -> o#add (Lfs.iroot, Lfs.empty_ext ()));
  };

  (* transaction part 2 *)
  let commit_and_make_new_transaction () = 
    Common.profile_code "Bdb commit" (fun () ->
      flush_db();
      Bdb.Txn.commit !t None;
      newt ();
    );
  in
  let abort_and_make_new_transaction () = 
    (* todo: but have to empty the cache too ? a #generic2 ? *)
    Bdb.Txn.abort !t;
    newt ();
  in

  let checkpoint_env () = Bdb.Txn.checkpoint env; in
  let log_archive_env () = Bdb.log_archive env in
  
  if use_transact
  then {
    Lfs_persistent.commit = commit_and_make_new_transaction; 
    abort = abort_and_make_new_transaction; 
    final = commit_and_make_new_transaction; 
    checkpoint = checkpoint_env; 
    archives = log_archive_env;
  }
  else {
    Lfs_persistent.commit = Common.do_nothing; 
    abort = Common.do_nothing; 
    final = 
      (* even if we dont use transaction, for the umount we must flush *)
      (fun () -> 
        flush_db();
        Bdb.Txn.commit !t None;
        Bdb.Db.close db1 [];
        Bdb.Db.close db2 [];
        Bdb.Db.close db3 [];
        Bdb.Db.close db4 [];
        Bdb.Db.close db5 [];
        Bdb.Db.close db6 [];
        Bdb.Db.close db7 [];
        Bdb.Env.close env [];
      );
    checkpoint = checkpoint_env; 
    archives = log_archive_env;
  }
