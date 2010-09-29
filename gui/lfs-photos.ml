open Common
(*
 lfs vs iphoto (et idive) =
  each action have repercursion on all the other windows (orthogonality)
  allow add additional prop to pwd, allow a browse from this too
   => have a generic query  and generic browse  (and browse from query, or query from browse)
*)

let path_xpm xpm = "/lfs-src/gui-xpms/" ^ xpm

  
(*******************************************************************************)
(* Common ui functions *)
(*******************************************************************************)
let dialog_text text title =
  let dialog = GWindow.dialog ~modal:true ~border_width:1 ~title:title () in
  let label  = GMisc.label    ~text:text     ~packing:dialog#vbox#add () in
  let dquit  = GButton.button ~label:"Close" ~packing:dialog#vbox#add () in 
  begin
    dquit#connect#clicked ~callback: (fun _ -> dialog#destroy ());
    dialog#show ();
  end

let freeze_thaw f l = 
  begin
    l#freeze ();
    f();
    l#thaw ();
  end

(*******************************************************************************)
(* The model *)
(*******************************************************************************)
type pwd = {
    mutable basic: string;

    mutable people: string list;
    mutable place:  string list;
    mutable event:  string list;
    mutable year:  string list;
    mutable month:  string list;
    (* old: string option *)
    
    mutable webalbum: string list;

    mutable additional: string;
    mutable google: string;
  } 

let pwd = {
  basic = "/";
  
  people = [];   place = [];   event = []; year = []; month = [];
  webalbum = [];

  additional = ""; google = "";
} 

let (string_of_pwd2: pwd -> string) = fun pwd -> 
  (if pwd.additional = "" then "" else pwd.additional ^ "/") ^
  (if pwd.google = "" then "" else "agrep:" ^ pwd.google ^ "/") ^
  ([
   (pwd.people,    "people:");
   (pwd.place,   "place:"); 
   (pwd.event,    "event:");
   (pwd.year,    "year:");
   (pwd.month,    "month:");
   (pwd.webalbum, "webalbum:")
   ] +> List.map (fun (xs, sattr) -> xs +> List.map (fun s -> sattr ^ s) +> String.concat "|")
     +> List.filter (fun s -> not (s = ""))
     +> String.concat "/")


let (string_of_pwd: pwd -> string) = fun pwd -> 
  pwd.basic ^ "/" ^ 
  (string_of_pwd2 pwd)

(*******************************************************************************)
let compute field = fun () ->
  "All"::(readdir_to_dir_list (string_of_pwd pwd ^ "/.relaxed/" ^ field ^ ":^/") +> List.map (fun s -> regexp_match s (field ^ ":\\(.*\\)")))
(* old: let compute_genre = fun () -> "All"::(readdir_to_dir_list (string_of_pwd pwd ^ "/.relaxed/genre:^/") +> List.map (fun s -> regexp_match s "genre:\\(.*\\)")) *)

let compute_objects = fun () ->
  (readdir_to_file_list (string_of_pwd pwd ^ "/.ext/") +> List.map (fun s -> s))

let compute_mv = fun src dst -> 
  ()

(* old: playlist_data +> ... *)
(* old:
let compute_playlist = fun () ->
  "All"::(readdir_to_dir_list "/" +> List.map (fun s -> "x" ^ (i_to_s !counter) ^ s))
let compute_genre = fun () ->
  "All"::(readdir_to_dir_list "/usr" +> List.map (fun s -> "x" ^ (i_to_s !counter) ^ s))
let compute_artist = fun () ->
  "All"::(readdir_to_dir_list "/etc" +> List.map (fun s -> "x" ^ (i_to_s !counter) ^ s))
let compute_album = fun () ->
  "All"::(readdir_to_dir_list "/proc" +> List.map (fun s -> "x" ^ (i_to_s !counter) ^ s))
*)


let cache_pixmap = Hashtbl.create 100


(*******************************************************************************)
(*******************************************************************************)
let refresh_all_func = ref []
let refresh_all () = 
  pr "refresh";
  !refresh_all_func +> List.iter (fun f -> f())


let file_selection_widget = ref None

type playmode = Normal | Random | Repeat
let playmode = new shared_variable_hook Normal

let next_track () = 
  let l = some !file_selection_widget in
  match l#selection with
  | [] -> 
      if playmode#get = Random 
      then l#select  (Random.int (l#rows)) 0
      else l#select 0 0
  | [i] -> 
      (match playmode#get with
      | Normal -> l#select (i+1) 0
      | Repeat -> l#select (i)   0
      | Random -> l#select  (Random.int (l#rows)) 0
      )
  | x::y::xs -> failwith "mutliple selection impossible here"

let previous_track () = 
  let l = some !file_selection_widget in
  match l#selection with
  | [] -> l#select 0 0
  | [i] -> 
      (match playmode#get with
      | Normal -> l#select (i-1) 0
      | Repeat -> l#select (i)   0
      | Random -> l#select (i)   0
      )
  | x::y::xs -> failwith "mutliple selection impossible here"
  
  
(*******************************************************************************)
(* External players variables and connection *)
(*******************************************************************************)
let process_current   = ref None

(* ugly hack, but have to know when sigchild because of dselect manual by user,  *)
(* or because of time off *)
let automatic_next = ref false

(* src: chailloux et al book *)
let rec sigchld_handle s = 
 try 
    let pid, _ = Unix.waitpid [Unix.WNOHANG] 0 in
    if pid <> 0 
    then
      begin 
        pr (Printf.sprintf "%d est mort et enterré au signal %d" pid s);
        if !automatic_next && !process_current != None && (some !process_current = pid) then
          begin
            process_current := None;
            next_track ();
          end;
        sigchld_handle s;
      end
 with Unix.Unix_error(_, "waitpid", _) -> pr "sigchld_handle error" (* exit 0  *)

let _ = Sys.set_signal Sys.sigchld (Sys.Signal_handle sigchld_handle)

(*******************************************************************************)
(* The Ui *)
(*******************************************************************************)

(* w = window, [hv]box = box, b = button, e = entry,  l = list, m = menu, mi = menuitem, r = range *)

let build_gui () =
  let w = GWindow.window ~title: "LFS GUI" ~width:800 ~height:600 (*~resizable:true*)  () in
  begin
    w#add (
        let vbox = GPack.vbox ~border_width:1 ~spacing:1 () in
        begin

          (*------------------------------------------------------------------------------*)
          (* Menu *)
          (*------------------------------------------------------------------------------*)
          vbox#pack (
              let m = GMenu.menu_bar () in
              let create_menu label =
                let item = GMenu.menu_item ~label ~packing:m#append () in
                GMenu.menu                        ~packing:item#set_submenu ()
              in
              let todo_gui () = dialog_text "This feature has not yet been implemented\nbut I encourage you to implement it yourself\nas there is very few chances that I do it one day\n" "TODO" in
              begin 
                GToolbox.build_menu (create_menu "Features") 
                  ~entries:
                  [
                    `I ("New Playlist",      todo_gui);
                    `I ("New SmartPlaylist", todo_gui);
                    `S;
                    `I ("Quit", GMain.Main.quit)
                  ];
                let ctr = (create_menu "Control") in
                GToolbox.build_menu ctr
                  ~entries:
                  [
                    `I ("Previous", previous_track);
                    `I ("Next",     next_track);
                    `S;
                  ];
                (*ctr#add ( *)
                    let mi = GMenu.check_menu_item ~label:"Random" ~packing:ctr#add () in
                    (* ugly hack, otherwise loop cos set_active call the callback toggled :( *)
                    let active_mode = ref true in 
                    begin
                      playmode#register (fun () -> 
                        active_mode := false;
                        mi#set_active (playmode#get = Random);
                        active_mode := true;
                      );
                      mi#connect#toggled ~callback:(fun () -> 
                        pr "toggled";
                        if !active_mode then
                        playmode#set (if playmode#get = Random then Normal else Random) 
                      );
                    end
                (* ); *) ;
                (* ctr#add ( *)
                    let mi = GMenu.check_menu_item ~label:"Repeat" ~packing:ctr#add () in
                    let active_mode = ref true in
                    begin
                      playmode#register (fun () -> 
                        active_mode := false;
                        mi#set_active (playmode#get = Repeat);
                        active_mode := true;
                      );
                      mi#connect#toggled ~callback:(fun () -> 
                        pr "toggled";
                        if !active_mode then
                        playmode#set (if playmode#get = Repeat then  Normal else Repeat) 
                      );
                      mi#coerce
                    end
                (* ); *) ;

                GToolbox.build_menu (create_menu "Help") 
                  ~entries:
                  [
                    `I ("Help on LFS", (fun () -> dialog_text "Read\nthe\nsource\n\ndude" "Help"));
                    `S;
                    `I ("About", (fun () -> dialog_text "Brought to you by padator\nwith love" "About"));
                  ];

                m#coerce
              end
          );


          (*------------------------------------------------------------------------------*)
          (* Commands *)
          (*------------------------------------------------------------------------------*)
          vbox#pack (
              let hbox = GPack.hbox ~border_width:1 ~spacing:1 () in
              begin
                hbox#pack (
                   let vbox =  GPack.vbox () in
                   begin
                     vbox#pack (
                        let hbox = GPack.hbox () in
                        begin
                          hbox#pack (
                             let b = GButton.button (* ~label:"Previous" *)  () in
                             let pixmap = GDraw.pixmap_from_xpm ~file:(path_xpm "previous2.xpm")  ~window:w () in
                             let _ = GMisc.pixmap pixmap ~packing:b#add () in
                             begin
                               b#set_relief `NONE;
                               b#connect#clicked ~callback:previous_track;
                               b#coerce
                             end
                           );
                          hbox#pack (
                               let b = GButton.button (*~label:"Play-Stop"*)  () in
                               let pixmap = GDraw.pixmap_from_xpm ~file:(path_xpm "play2.xpm")  ~window:w () in
                               let _ = GMisc.pixmap pixmap ~packing:b#add () in
                               begin
                                 b#set_relief `NONE;
                                 b#connect#clicked ~callback: (fun () -> 
                                   match !process_current with
                                   | None -> ()
                                   | Some id -> 
                                       automatic_next := false;
                                       Unix.kill id Sys.sigint; 
                                            (*old: Unix.sleep 1;*)
                                       ignore(Sys.command ("usleep 500000"));  
                                       process_current := None
                                  );

                                 b#coerce
                               end
                           );
                          hbox#pack (
                                let b = GButton.button (*~label:"Next"*)  () in
                                let pixmap = GDraw.pixmap_from_xpm ~file:(path_xpm "forward2.xpm")  ~window:w () in
                                let _ = GMisc.pixmap pixmap ~packing:b#add () in
                                begin
                                  b#set_relief `NONE;
                                  b#connect#clicked ~callback:next_track;
                                  b#coerce
                                end
                           );
                          hbox#coerce
                        end
                     );
                     vbox#coerce
                   end
                );


                hbox#pack ~expand:true (
                  let frame = GBin.frame  (*~width:100*) () in
                  let box =  GPack.hbox ~packing:frame#add () in
                  let e =  GMisc.label ~text:"pwd:" ~packing:box#pack () in
                  let e =  GMisc.label ~text:"" (*~width:60*) ~packing:(box#pack (*~expand:true*)) 
                      (*~line_wrap:true*)
                      () in
                  begin
                    let compute_text_pwd = fun () -> e#set_text (string_of_pwd2 pwd) in
                    refresh_all_func := compute_text_pwd :: !refresh_all_func;
                    frame#coerce
                  end
                );

                hbox#pack ~from:`END (
                     let e =  GEdit.entry ~text:"" ~editable:true ~max_length: 60 () in
                     begin
                       e#connect#activate ~callback:(fun () -> 
                         pr ("Entry contents:" ^ e#text);
                         pwd.google <- e#text;
                         refresh_all ();
                       );
                       e#coerce
                     end
                );
                hbox#pack ~from:`END ( (GMisc.label ~text:"search:" ())#coerce);

                hbox#pack ~from:`END (
                     let e =  GEdit.entry ~text:"" ~editable:true ~max_length: 30 () in
                     begin
                       e#connect#activate ~callback:(fun () -> 
                         pr ("Entry contents:" ^ e#text);
                         pwd.additional <- e#text;
                         refresh_all ();
                       );
                       e#coerce
                     end
                );
                hbox#pack ~from:`END ( (GMisc.label ~text:"additional:" ())#coerce );

                hbox#coerce
              end
          );


          (*------------------------------------------------------------------------------*)
          (* Browser *)
          (*------------------------------------------------------------------------------*)
          vbox#pack ~expand:true ~fill:true (
              let hpaned = GPack.paned `HORIZONTAL () in
              begin

                (*------------------------------------------------------------------------------*)
                (* Playlist *)
                (*------------------------------------------------------------------------------*)
                hpaned#add1 (
                    let l = GList.clist ~titles:["";"WebAlbum"] ~width:100 (* ~height:300*) ~border_width:4 
                        ~selection_mode: `EXTENDED
                        ~row_height:19
                        () in
                    let scrw = GBin.scrolled_window ~border_width:0 ~width:130 ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC  () in
                    let _ = scrw#add_with_viewport l#coerce in
                    let pixmap =     GDraw.pixmap_from_xpm ~file:(path_xpm "playlist.xpm")  ~window:w () in
                    let pixmap_lib = GDraw.pixmap_from_xpm ~file:(path_xpm "library.xpm")   ~window:w () in
                    let _ = l#set_column ~width:20 0 in
                    begin
                      let compute_ui_webalbum = fun () -> 
                        (* refresh only if have selected All, otherwise keep current selection *)
                        if pwd.webalbum = []
                        then
                          begin 
                            l +> freeze_thaw (fun () ->
                            l#clear ();
                            (compute "webalbum") () +> index_list +> List.iter (fun (s, i) -> 
                               ignore (l#append ["fake for pixmap later";s]); 
                            );
                            (compute "webalbum") () +> index_list +> List.iter (fun (s, i) -> 
                              l#set_cell ~text:s i 1;
                              l#set_cell ~pixmap:(if i = 0 then pixmap_lib else pixmap) i 0;
                            );
                            );
                          end
                        else ()
                      in
                      compute_ui_webalbum ();
                      (* CONFIG, better for dnd to comment this line *)
                      (*refresh_all_func := compute_ui_playlist :: !refresh_all_func; *)

                      l#drag#dest_set ~actions:[`COPY; `MOVE] [ { Gtk.target = "STRING"; Gtk.flags = []; Gtk.info = 0}];
                      l#drag#connect#data_received ~callback: 
                        (fun context ~x ~y sel ~info ~time -> 
                          try 
                            (* ugly hack, use the -19 trick (cf google:gtk drag drop clist) => must be equal to row-height set before *)
                            let (row, column) = l#get_row_column x (max 0 (y - 19)) in
                            let (src, dst)    = (sel#data, l#cell_text row 1) in
                            pr "dnd: received";
                            pr (Printf.sprintf "row=%d; column=%d" row column);
                            pr (Printf.sprintf "dnd: received = '%s'   that will go in '%s'" src dst);
                            compute_mv src dst;
                            context#finish ~success:true ~del:false ~time
                          with _ -> pr "Drag and drop failure"
                        );

                   
                      l#connect#select_row ~callback:
                        (fun ~row ~column ~event ->
                          pr (Printf.sprintf "xrow=%d, column=%d" row column);
                          pr ("xtree selection:" ^ l#selection +> List.map (fun i -> (i_to_s i)) +> String.concat "," );
                          l#selection +> List.iter (fun row -> pr (l#cell_text row 1));
                          (* selection trick, ugly hack *)
                          if row < 100000 then (
                            if (l#selection +> List.exists (fun row -> (l#cell_text row 1) = "All")) 
                            then pwd.webalbum <- []
                            else pwd.webalbum <- (l#selection +> List.map (fun row -> l#cell_text row 1));
                            refresh_all ();
                           )
                        );

                      l#connect#unselect_row ~callback:
                        (fun ~row ~column ~event ->
                          pr (Printf.sprintf "xtext unselected:row=%d:col=%d" row column);
                          l#selection +> List.iter (fun row -> pr ("  unselect ->" ^ (l#cell_text row 1)));
                          pwd.webalbum <- pwd.webalbum +> remove (l#cell_text row 1);
                          (* selection trick, ugly hack *)
                          if pwd.webalbum = [] then () else refresh_all ()
                        );

                      scrw#coerce
                    end
                );
                
                hpaned#add2  (
                     let vpaned = GPack.paned `VERTICAL () in
                     begin

                       (*------------------------------------------------------------------------------*)
                       (* Selection Columns *)
                       (*------------------------------------------------------------------------------*)
                       vpaned#add1  (
                           let hbox = GPack.hbox ~border_width:1 ~spacing:1 () in
                           begin

                           let build_column column_string  pwd_field assign_pwd_field compute_field =
                               
                             hbox#pack ~expand:true ~fill:true (
                                 let l = GList.clist ~titles:[column_string] (* ~width:100 ~height:300*) ~border_width:4 
                                     ~selection_mode: `EXTENDED
                                     () in
                                 let scrw = GBin.scrolled_window ~border_width: 0 ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC  () in
                                 let _ = scrw#add_with_viewport l#coerce in
                                 begin
                                   let compute_ui_field = fun () -> 
                                     (* refresh only if have selected All, otherwise keep current selection *)
                                     if pwd_field () = []
                                     then
                                       begin 
                                         l +> freeze_thaw (fun () ->
                                         l#clear ();
                                         compute_field () +> List.iter (fun s -> ignore (l#append [s]));
                                         );
                                       end
                                     else ()
                                   in
                                   compute_ui_field ();
                                   refresh_all_func := compute_ui_field :: !refresh_all_func;

                                   l#connect#select_row ~callback:
                                     (fun ~row ~column ~event ->
                                       pr (Printf.sprintf "xrow=%d, column=%d" row column);
                                       pr ("xtree selection:" ^ l#selection +> List.map (fun i -> (i_to_s i)) +> String.concat "," );
                                       l#selection +> List.iter (fun row -> pr (l#cell_text row 0));
                                       (* selection trick, ugly hack *)
                                       if row < 100000 then (
                                         if (l#selection +> List.exists (fun row -> (l#cell_text row 0) = "All")) 
                                         then assign_pwd_field []
                                         else assign_pwd_field (l#selection +> List.map (fun row -> l#cell_text row 0));
                                         refresh_all ();
                                        )
                                     );

                                   l#connect#unselect_row ~callback:
                                     (fun ~row ~column ~event ->
                                       pr (Printf.sprintf "xtext unselected:row=%d:col=%d" row column);
                                       l#selection +> List.iter (fun row -> pr ("  unselect ->" ^ (l#cell_text row 0)));
                                       assign_pwd_field (pwd_field () +> remove (l#cell_text row 0));
                                       (* selection trick, ugly hack *)
                                       if pwd_field() = [] then () else refresh_all ()
                                     );

                                   scrw#coerce
                                 end
                             )
                             in
                             build_column "People"  (fun () -> pwd.people)  (fun x -> pwd.people  <- x) (compute "people");
                             build_column "Place" (fun () -> pwd.place) (fun x -> pwd.place <- x) (compute "place");
                             build_column "Event"  (fun () -> pwd.event)  (fun x -> pwd.event  <- x) (compute "event");
                             build_column "Year"  (fun () -> pwd.year)  (fun x -> pwd.year  <- x) (compute "year");
                             build_column "Month" (fun () -> pwd.month) (fun x -> pwd.month <- x) (compute "month");

                             hbox#coerce
                           end
                       );

                       (*------------------------------------------------------------------------------*)
                       (* Objects list *)
                       (*------------------------------------------------------------------------------*)
                       vpaned#add2 (
                            let l = GList.clist ~titles:["Filename"; "xxx"] (* ~width:100 ~height:300*) ~border_width:4 
                                ~selection_mode:`SINGLE
                                ~row_height:80
                                () in
                            let scrw = GBin.scrolled_window ~border_width: 0 ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC  () in
                            let _ = scrw#add_with_viewport l#coerce in
                            let pixmap =     GDraw.pixmap_from_xpm ~file:(path_xpm "playlist.xpm")  ~window:w () in
                            begin
                              let compute_ui_objects = fun () -> 
                                l +> freeze_thaw (fun () ->
                                l#clear ();
                                compute_objects () +> index_list +> List.iter (fun (s, i) -> 
                                  ignore (l#append [s; "fake for pixmap later"]); 
                                );
                                compute_objects () +> index_list +> List.iter (fun (s, i) -> 
                                  l#set_cell ~text:s i 0;
                                  l#set_cell ~pixmap:
                                    (cache_pixmap +> find_hash_set s 
                                       (fun () -> 
                                         let filename = (string_of_pwd pwd ^ "/.ext/") ^ "/" ^ s in
                                         let pbx = GdkPixbuf.from_file filename in
                                         let dest = GdkPixbuf.create ~width:80 ~height:80 () in
                                         let _ = GdkPixbuf.scale ~dest:dest ~width:80 ~height:80 pbx  in
                                         let pm, _ = GdkPixbuf.create_pixmap dest in
                                         (new GDraw.pixmap pm) 
                                           )
                                    ) i 1;
                                );
                                );
                              in
                              compute_ui_objects ();
                              refresh_all_func := compute_ui_objects :: !refresh_all_func;
                              file_selection_widget := Some l;

                              l#drag#source_set ~modi:[`BUTTON1] ~actions:[`COPY] [ { Gtk.target = "STRING"; Gtk.flags = []; Gtk.info = 0}];
                              l#drag#connect#data_get ~callback:(fun _ sel ~info ~time -> 
                                pr "dnd: get";
                                sel#return (l#cell_text l#focus_row 0);
                                );
                              (*l#drag#source_set_icon pixmap;*)

                              l#connect#select_row ~callback:
                                (fun ~row ~column ~event ->
                                  let text = l#cell_text row column in
                                  begin
                                    pr (Printf.sprintf "text selected:row=%d:col=%d:%s" row column text);
                                  end
                               );

                              l#connect#unselect_row ~callback:
                                (fun ~row ~column ~event ->
                                  pr (Printf.sprintf "text unselected:row=%d:col=%d" row column);
                                  );

                              l#columns_autosize ();
                              scrw#coerce
                            end
                        );
                       vpaned#coerce
                     end
                );
                hpaned#coerce
              end
          );





          (*------------------------------------------------------------------------------*)
          (* Status *)
          (*------------------------------------------------------------------------------*)
          vbox#pack ~from: `END (
              let hbox = GPack.hbox ~border_width:1 ~spacing:1 () in
              begin
                hbox#pack (
                     let b = GButton.button (*~label:"Random"*) () in
                     let pix_off = GDraw.pixmap_from_xpm ~file:(path_xpm "random.xpm")  ~window:w () in
                     let pix_on  = GDraw.pixmap_from_xpm ~file:(path_xpm "random-activated2.xpm")  ~window:w () in
                     let cell_pix = GMisc.pixmap pix_off ~packing:b#add () in
                     let _ = (GData.tooltips ())#set_tip b#coerce ~text:"random playing" in
                     begin
                       b#set_relief `NONE;
                       playmode#register (fun () -> 
                         cell_pix#set_pixmap (if playmode#get = Random then pix_on else pix_off)
                       );
                       b#connect#clicked ~callback: (fun () -> 
                         playmode#set (if playmode#get = Random then  Normal else Random)
                       );
                       b#coerce
                     end
                );
                hbox#pack (
                     let b = GButton.button (*~label:"Loop"*) () in
                     let pix_off = GDraw.pixmap_from_xpm ~file:(path_xpm "repeat.xpm")  ~window:w () in
                     let pix_on  = GDraw.pixmap_from_xpm ~file:(path_xpm "repeat-activated2.xpm")  ~window:w () in
                     let cell_pix = GMisc.pixmap pix_off ~packing:b#add () in
                     let _ = (GData.tooltips ())#set_tip b#coerce ~text:"repeat playing" in
                     begin
                       b#set_relief `NONE;
                       playmode#register (fun () -> 
                         cell_pix#set_pixmap (if playmode#get = Repeat then pix_on else pix_off)
                       );
                       b#connect#clicked ~callback: (fun () -> 
                         playmode#set (if playmode#get = Repeat then Normal else Repeat)
                       );
                       b#coerce
                     end
                );
                hbox#pack ~expand:true (
                     let e =  GMisc.label ~text:"status" ~width:60 () in
                     begin
                       let compute_text_status = fun () -> 
                         e#set_text (Printf.sprintf "status: %d pictures"  
                                       (compute_objects () +> List.length)
                                    )
                       in
                       compute_text_status ();
                       refresh_all_func := compute_text_status :: !refresh_all_func;
                       e#coerce
                     end
                );
                hbox#coerce
              end
          );
          vbox#coerce
        end
    );
    w#event#connect#delete ~callback:(fun _ -> GMain.Main.quit (); true);
    w#connect#destroy      ~callback:          GMain.Main.quit;
    w#show ();
    GMain.Main.main ()
  end



(*******************************************************************************)
let main () = 
  begin
    pwd.basic <- Sys.getcwd ();
    pr (string_of_pwd pwd);
    build_gui ();
  end

let _ = main ()
