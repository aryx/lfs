open Common open Commonop

(* lablgtk *)

(*****************************************************************************)
(*
 * usage:
 *  ./lfs-tunes -path_xpm_pixmaps /path/to/gui-xpms/ -sleep_time 1000000000
 *
 * lfs-tunes vs iTunes:
 *  Each action have repercursion on all the other windows (orthogonality)
 *  (in fact itunes can do that too a little, can browse from query, have
 *   to go in special menu to reput the browsers windows)
 *  Allow add additional prop to pwd, allow a browse from this too
 *  => have a generic query  and generic browse
 *  (and browse from query, or query from browse)
 *
 * note: archi: note that this application is not dependent on LFS source.
 *
 * related:
 *  - digital dj (based on sql),
 *  - mp3kult (bof)
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let _path_xpm = ref "/home/pad/lfs_code/gui/gui-xpms/"
let _sleep_time = ref "500000"

let path_xpm xpm =  !_path_xpm ^ xpm

let sleep_little () =
  (*old:  *)
  Unix.sleep 1
  (*ignore(Sys.command ("usleep " ^ !_sleep_time))*)

(*****************************************************************************)
(* Common ui functions *)
(*****************************************************************************)
let dialog_text text title =
  let dialog = GWindow.dialog ~modal:true ~border_width:1 ~title:title () in
  let _label  = GMisc.label    ~text:text     ~packing:dialog#vbox#add () in
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

let todo_gui () =
  begin
    dialog_text "This feature has not yet been implemented
but I encourage you to implement it yourself
as there is very few chances that I do it one day"

    "TODO"
  end

(*****************************************************************************)
(* The model *)
(*****************************************************************************)
type pwd = {
  mutable basic: string;

  mutable genre:  string list;
  mutable artist: string list;
  mutable album:  string list;
  (* old: string option *)

  mutable playlist: string list;

  mutable additional: string;
  mutable google: string;
}

let pwd = {
  basic = "/";

  genre = [];   artist = [];   album = [];
  playlist = [];

  additional = ""; google = "";
}

let (string_of_pwd2: pwd -> string) = fun pwd ->
  (if pwd.additional = "" then "" else pwd.additional ^ "/") ^
  (if pwd.google = ""     then "" else "stree:" ^ pwd.google ^ "/") ^
  ([
    (pwd.genre,    "genre:");
    (pwd.artist,   "artist:");
    (pwd.album,    "album:");
    (pwd.playlist, "playlist:");
  ] +> List.map (fun (xs, sattr) ->
         xs +> List.map (fun s -> sattr ^ s) +> String.concat "|"
    )
    +> List.filter (fun s -> not (s = ""))
    +> String.concat "/")


let (s_of_pwd: pwd -> string) = fun pwd ->
  pwd.basic ^ "/" ^ (string_of_pwd2 pwd)

(*****************************************************************************)
let compute_playlist = fun () ->
  "All"::(readdir_to_dir_list (s_of_pwd pwd ^ "/.classic/playlist:^/")
  +> List.map (fun s -> regexp_match s "playlist:\\(.*\\)"))
let compute_genre = fun () ->
  "All"::(readdir_to_dir_list (s_of_pwd pwd ^ "/.relaxed/genre:^/")
  +> List.map (fun s -> regexp_match s "genre:\\(.*\\)"))
let compute_artist = fun () ->
  "All"::(readdir_to_dir_list (s_of_pwd pwd ^ "/.relaxed/artist:^/")
  +> List.map (fun s -> regexp_match s "artist:\\(.*\\)"))
let compute_album = fun () ->
  "All"::(readdir_to_dir_list (s_of_pwd pwd ^ "/.relaxed/album:^/")
  +> List.map (fun s -> regexp_match s "album:\\(.*\\)"))

let compute_objects = fun () ->
  (readdir_to_file_list (s_of_pwd pwd ^ "/.ext/")
   +> List.map (fun s -> [s; "1"]))

(* todo: if have formula ? => need a .only_leaf *)
let compute_length = fun () ->
  readdir_to_dir_size_list (s_of_pwd pwd ^ "/.relaxed/time:^/")
    +> List.map (fun (s,i) ->
      if s =~ "time:\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
      then
        let (min, sec) = matched2 s +> pair s_to_i in
        (min * 60 + sec) * i
      else 0
     )
    +> List.fold_left (+) 0

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

(*****************************************************************************)
(* The Controler *)
(*****************************************************************************)
let refresh_all_func = ref []
let refresh_all () =
  pr "refresh";
  !refresh_all_func +> List.iter (fun f -> f())


let file_selection_widget = ref None

type playmode = Normal | Random | Repeat
let playmode = new shared_variable_hook Normal

let _tracks_history         = ref ([]: int list)
let _tracks_forward_history = ref ([]: int list)

let _lastime_played = ref (Unix.time ())
let _go_previous_threshold = ref 2.0

(* note: note that this function call l#select and so the handle of the select windows *)
(*  hence for instance the automatic_next is reset to true at each call to next_track or previous_track *)
let next_track () =
  let l = some !file_selection_widget in
  let next_song =
(* TODO
      match l#selection with
      | [] ->
          if playmode#get = Random
          then Random.int (l#rows)
          else 0
      | [i] ->
          push2 i _tracks_history;
          (match playmode#get with
          | Normal -> (i+1)
          | Repeat -> i
          | Random ->
              if !_tracks_forward_history = []
              then Random.int (l#rows)
              else pop2 _tracks_forward_history
          )
      | x::y::xs -> failwith "mutliple selection impossible here"
*)
    raise Todo

  in
  l#select next_song 0


(* todo? allow play forward too even when random ? *)
(* todo: catch some exn with pop2 *)
(* todo: a mon avis lorsque la liste change de taille, le i dans les history sont plus valides *)
(*  et faudrait donc mieux resetter la liste *)
let previous_track () =
  let l = some !file_selection_widget in
  let next_song =
    (* as in itunes, previous is overloaded, different behaviour en fonction de  *)
    (*  are we at beginning of song *)
(* TODO
    match l#selection with
    | [] -> 0
    | [i] ->
        if (Unix.time () -. !_lastime_played > !_go_previous_threshold)
        then i
        else
          (match playmode#get with
          | Normal -> i-1
          | Repeat -> i
          | Random ->
              push2 i _tracks_forward_history;
              pop2 _tracks_history;
          )
  | x::y::xs -> failwith "mutliple selection impossible here"
*)
    raise Todo
  in
  l#select next_song 0


(*****************************************************************************)
(* External players variables and connection *)
(*****************************************************************************)
let process_current   = ref None
let process_equalizer = ref None

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

(*****************************************************************************)
(* The Ui *)
(*****************************************************************************)

(* w = window, [hv]box = box,
 * b = button, e = entry,  l = list, m = menu, mi = menuitem, r = range
 *)

let build_gui () =
  let w = GWindow.window ~title: "LFS GUI" ~width:800 ~height:600
    (*~resizable:true*)  ()
  in
  begin
    w#add (
      let vbox = GPack.vbox ~border_width:1 ~spacing:1 () in
      begin

        (*-------------------------------------------------------------------*)
        (* Menu *)
        (*-------------------------------------------------------------------*)
        vbox#pack (
          let m = GMenu.menu_bar () in
          let create_menu label =
            let item = GMenu.menu_item ~label ~packing:m#append () in
            GMenu.menu                        ~packing:item#set_submenu ()
          in
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
            (*ctr#add () *)
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
              (* (); *) ;
              (* ctr#add () *)
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
              (* (); *) ;

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


        (*-------------------------------------------------------------------*)
        (* Commands *)
        (*-------------------------------------------------------------------*)
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
                        b#connect#clicked ~callback: previous_track;
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
                              sleep_little();
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
                vbox#pack (
                  let adj = GData.adjustment ~lower:0.0 ~upper:100.0 ~page_size:1.0 () in
                  let r = GRange.scale  `HORIZONTAL ~adjustment:adj ~draw_value:false () in
                  begin
                    (* ugly hack, but seems that process_output-to-list interact badly *)
                    Sys.set_signal Sys.sigchld (Sys.Signal_default);
                    adj#set_value (
                      match process_output_to_list "aumix -w q" with
                      | [x] when x =~ "pcm \\([0-9]+\\)," ->
                          matched1 x +> s_to_i +> float_of_int
                      | _ -> pr "error: cant get mix value, default to 0"; 0.0
                    );
                    Sys.set_signal Sys.sigchld (Sys.Signal_handle sigchld_handle);
                    adj#connect#value_changed ~callback: (fun () ->
                      pr (i_to_s (int_of_float adj#value));
                      ignore(Sys.command("aumix -w " ^ (i_to_s (int_of_float adj#value))));
                    );
                    r#coerce
                  end
                );
                vbox#coerce
              end
            );


            hbox#pack ~expand:true (
              let frame = GBin.frame  (*~width:100*) () in
              let box =  GPack.hbox ~packing:frame#add () in
              let _e =  GMisc.label ~text:"pwd:" ~packing:box#pack () in
              let e =  GMisc.label ~text:"" (*~width:60*) ~packing:(box#pack (*~expand:true*))
                (*~line_wrap:true*)
                () in
              begin
                let compute_text_pwd = fun () -> e#set_text (s_of_pwd pwd) in
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
                e#connect#changed ~callback:(fun () ->
                  pr ("Entry contents:" ^ e#text);
                  if (String.length e#text >= 4 || String.length e#text = 0) then
                    begin
                      pwd.google <- e#text;
                      refresh_all ();
                    end;

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


        (*-------------------------------------------------------------------*)
        (* Browser *)
        (*-------------------------------------------------------------------*)
        vbox#pack ~expand:true ~fill:true (
          let hpaned = GPack.paned `HORIZONTAL () in
          begin

            (*------------------------------------------------------------------------------*)
            (* Playlist *)
            (*------------------------------------------------------------------------------*)
            hpaned#add1 (
              let l = GList.clist ~titles:["";"Playlist"] ~width:100 (* ~height:300*) ~border_width:4
                ~selection_mode: `MULTIPLE (* EXTENDED *)
                ~row_height:19
                () in
              let scrw = GBin.scrolled_window ~border_width:0 ~width:130 ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC  () in
              let _ = scrw#add_with_viewport l#coerce in

              let pixmap =     GDraw.pixmap_from_xpm ~file:(path_xpm "playlist.xpm")  ~window:w () in
              let pixmap_lib = GDraw.pixmap_from_xpm ~file:(path_xpm "library.xpm")   ~window:w () in
              let _ = l#set_column ~width:20 0 in
              begin
                let compute_ui_playlist = fun () ->
                  (* refresh only if have selected All, otherwise keep current selection *)
                  if pwd.playlist = []
                  then
                    begin
                      l +> freeze_thaw (fun () ->
                        l#clear ();
                        compute_playlist () +> index_list +> List.iter (fun (s, i) ->
                          ignore (l#append ["fake for pixmap later";s]);
                        );
                        compute_playlist () +> index_list +> List.iter (fun (s, i) ->
                          l#set_cell ~text:s i 1;
                          l#set_cell ~pixmap:(if i = 0 then pixmap_lib else pixmap) i 0;
                        );
                      );
                          end
                  else ()
                in
                compute_ui_playlist ();
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
(* TODO
                    pr ("xtree selection:" ^ l#selection +> List.map (fun i -> (i_to_s i)) +> String.concat "," );
                    l#selection +> List.iter (fun row -> pr (l#cell_text row 1));
                    (* selection trick, ugly hack *)
                    if row < 100000 then (
                      if (l#selection +> List.exists (fun row -> (l#cell_text row 1) = "All"))
                      then pwd.playlist <- []
                      else pwd.playlist <- (l#selection +> List.map (fun row -> l#cell_text row 1));
                      refresh_all ();
                    )
*)
                  );

                l#connect#unselect_row ~callback:
                  (fun ~row ~column ~event ->
                    pr (Printf.sprintf "xtext unselected:row=%d:col=%d" row column);
(* TODO
                    l#selection +> List.iter (fun row -> pr ("  unselect ->" ^ (l#cell_text row 1)));
*)
                    pwd.playlist <- pwd.playlist +> remove (l#cell_text row 1);
                    (* selection trick, ugly hack *)
                    if pwd.playlist = [] then () else refresh_all ()
                  );
                scrw#coerce
              end
            );
            hpaned#add2  (
              let vpaned = GPack.paned `VERTICAL () in
              begin

                (*----------------------------------------------------*)
                (* Selection Columns *)
                (*----------------------------------------------------*)
                vpaned#add1  (
                  let hbox = GPack.hbox ~border_width:1 ~spacing:1 () in
                  begin

                    let build_column column_string  pwd_field assign_pwd_field compute_field =

                      hbox#pack ~expand:true ~fill:true (
                        let l = GList.clist ~titles:[column_string] (* ~width:100 ~height:300*) ~border_width:4
                          ~selection_mode: `MULTIPLE (* `EXTENDED *)
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
(* TODO
                              pr ("xtree selection:" ^ l#selection +> List.map (fun i -> (i_to_s i)) +> String.concat "," );
                              l#selection +> List.iter (fun row -> pr (l#cell_text row 0));
                              (* selection trick, ugly hack *)
                              if row < 100000 then (
                                if (l#selection +> List.exists (fun row -> (l#cell_text row 0) = "All"))
                                then assign_pwd_field []
                                else assign_pwd_field (l#selection +> List.map (fun row -> l#cell_text row 0));
                                refresh_all ();
                              )
*)
                            );

                          l#connect#unselect_row ~callback:
                            (fun ~row ~column ~event ->
                              pr (Printf.sprintf "xtext unselected:row=%d:col=%d" row column);
(* TODO
                              l#selection +> List.iter (fun row -> pr ("  unselect ->" ^ (l#cell_text row 0)));
*)
                              assign_pwd_field (pwd_field () +> remove (l#cell_text row 0));
                              (* selection trick, ugly hack *)
                              if pwd_field() = [] then () else refresh_all ()
                            );

                          scrw#coerce
                        end
                      )
                    in
                    build_column "Genre"  (fun () -> pwd.genre)  (fun x -> pwd.genre  <- x) compute_genre;
                    build_column "Artist" (fun () -> pwd.artist) (fun x -> pwd.artist <- x) compute_artist;
                    build_column "Album"  (fun () -> pwd.album)  (fun x -> pwd.album  <- x) compute_album;

                    hbox#coerce
                  end
                );

                (*------------------------------------------------*)
                (* Objects list *)
                (*------------------------------------------------*)
                vpaned#add2 (
                  let l = GList.clist ~titles:["Filename"; "xxx"] (* ~width:100 ~height:300*) ~border_width:4
                    ~selection_mode:`SINGLE
                    () in
                  let scrw = GBin.scrolled_window ~border_width: 0 ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC  () in
                  let _ = scrw#add_with_viewport l#coerce in
                  begin
                    let compute_ui_objects = fun () ->
                      l +> freeze_thaw (fun () ->
                        l#clear ();
                        compute_objects () +> List.iter (fun xs -> ignore (l#append xs));
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
                          if column = 0 then
                            begin
                              (match !process_current with
                              | None -> ()
                              | Some id ->
                                  automatic_next := false;
                                  Unix.kill id Sys.sigint;
                                  sleep_little();
                              );

                              let prog = (
                                match text with
                                | s when s =~ ".*.ogg$" -> "ogg123"
                                | s when s =~ ".*.mp3$" -> "mpg123"
                                | s when s =~ ".*.mov" -> "mplayer"
                                | s when s =~ ".*.wma" -> "mplayer"
                                | s when s =~ ".*.mod" -> "mikmod"
                                | s when s =~ ".*.s3m" -> "mikmod"
                                | s when s =~ ".*.xm" -> "mikmod"
                                | s when s =~ ".*.it" -> "mikmod"
                                | _ -> failwith ("dont handle this kind of file:" ^ text)
                              ) in

                              (*old: let command = "ogg123 " ^ (s_of_pwd pwd ^ "/.ext/") ^ "/" ^ text in *)

                              process_current := Some
                                (Unix.create_process
                                    prog
                                    [|prog; (s_of_pwd pwd ^ "/.ext/") ^ "/" ^ text |]
                                    Unix.stdin Unix.stdout Unix.stderr
                                );
                              _lastime_played := Unix.time ();
                              automatic_next := true;
                            end
                        end);

                    l#connect#unselect_row ~callback:
                      (fun ~row ~column ~event ->
                        pr (Printf.sprintf "text unselected:row=%d:col=%d" row column);
                        (match !process_current with
                        | None -> ()
                        | Some id ->
                            automatic_next := false;
                            Unix.kill id Sys.sigint;
                            sleep_little();
                            process_current := None
                        );
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





        (*-------------------------------------------------------------------*)
        (* Status *)
        (*-------------------------------------------------------------------*)
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
                  e#set_text (Printf.sprintf "status: %d songs,  %s"
                                 (compute_objects () +> List.length)
                                 (compute_length () +> sec_to_days)
                  )
                in
                compute_text_status ();
                refresh_all_func := compute_text_status :: !refresh_all_func;
                e#coerce
              end
            );
            hbox#pack ~from:`END (
              let b = GButton.toggle_button (*~label:"equalizer"*) () in
              let pix_off = GDraw.pixmap_from_xpm ~file:(path_xpm "equalizerx.xpm")  ~window:w () in
              let pix_on  = GDraw.pixmap_from_xpm ~file:(path_xpm "equalizer.xpm")  ~window:w () in
              let cell_pix = GMisc.pixmap pix_off ~packing:b#add () in
              begin
                b#set_relief `NONE;
                b#connect#toggled ~callback: (fun () ->
                  cell_pix#set_pixmap (if b#active then pix_on else pix_off);
                  pr "equalizer";
                  (match !process_equalizer with
                  | None ->
                      process_equalizer := Some
                        (Unix.create_process
                            "aumix"
                            [|"aumix" |]
                            Unix.stdin Unix.stdout Unix.stderr
                        );
                  | Some id ->
                      Unix.kill id Sys.sigint;
                      process_equalizer := None
                         );
                );
                b#coerce
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



(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let main () =
  begin
    let args = ref [] in
    let rec options = Arg.align
      [
        "-sleep_time"       , Arg.Set_string  _sleep_time,
        " sleep time (in ns)";
        "-path_xpm_pixmaps" , Arg.Set_string  _path_xpm,
        " path to pixmaps icons files";
      ]
    in
    let usage_msg =
      ("Usage: " ^ basename Sys.argv.(0) ^ " [options] \nOptions are:")
    in

    Arg.parse options (fun file -> args := file::!args) usage_msg;
    args := List.rev !args;
    (match (!args) with
    | [] -> () (* usage_function() *)
    | x::xs -> ()
    );

    pwd.basic <- Sys.getcwd ();
    pr (s_of_pwd pwd);

    build_gui ();
  end

(*****************************************************************************)
let _ =
  main ()
