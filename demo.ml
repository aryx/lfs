open Common

open Lfs
open Path

open Plugins
(* open Ioplugins *)

(*****************************************************************************)
(* note: to debug can be helpful to put some  'end let res () =  begin'
 * in the middle of a demo to end the demo at that point.
 *
 * could: as work with pure data-structure, could do a function  undo;
 * just need to save the old world and reassign back !Lfs.w .
 *)

(*****************************************************************************)
let ls_ () = Lfs.ls_bis ()
let ls_print () = ls_() +> List.iter print_endline

(*****************************************************************************)
let principles_demo () =
  begin
    (*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
    (* \subsection{On files} *)
    (*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

    (*--------------------------------------------------------------*)
    (* \subsubsection{Organise files} *)
    (*--------------------------------------------------------------*)
    mkdir "art";
    mkdir "music";
    mkdir "movie";
    mkdir "port";
    mkdir "seaside";
    mkdir "USA";
    mkdir "capital";

    (*--------------------------------------------------------------*)
    cd_ "/art/movie/port/seaside/USA"; mkfile_ "los-angeles.jpg" "";
    cd_ "/USA/capital";                mkfile_ "washington.jpg" "";
    cd_ "/port/seaside/USA";           mkfile_ "miami.jpg" "";
    cd_ "/art/music/port/seaside/USA"; mkfile_ "boston.jpg" "";
    cd_ "/port";                       mkfile_ "hamburg.jpg" "";
    cd_ "/port/seaside/USA";           mkfile_ "san-diego.jpg" "";
    cd_ "/art/music/port/seaside/USA"; mkfile_ "new-york.jpg" "";

    (*--------------------------------------------------------------*)
    (* \subsubsection{Retrieve files} *)
    (*--------------------------------------------------------------*)
    cd_ "/port/USA";
    assert(ls_() $=$ (set
			[ "music/"; "movie/"; "art/";
			  "miami.jpg"; "san-diego.jpg"]));

    (*--------------------------------------------------------------*)
    cd_ "/capital|movie/!seaside";
    assert(ls_() $=$ (set ["washington.jpg"]));

    (*--------------------------------------------------------------*)
    (* \subsubsection{Manipulate files} *)
    (*--------------------------------------------------------------*)
    rm (Left "washington.jpg") +> ignore;

    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "hot";
    cd_ "port/USA";
    assert(ls_() $=$ (set
			[ "music/"; "movie/"; "art/";
			  "miami.jpg"; "san-diego.jpg"]));
    mv_ "miami.jpg" "../hot";
    (* todo: assert not anymore in USA, and in hot, and still in seaside *)

    (*--------------------------------------------------------------*)
    (* \subsubsection{Organise properties manually} *)
    (*--------------------------------------------------------------*)
    cd_ "/";
    rmdir "movie";
    rmdir "music";
    rmdir "art";

    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "art";
    cd_ "art";
    mkdir "music";
    mkdir "movie";

    (*--------------------------------------------------------------*)
    cd_ "/port&USA";
    assert(ls_() $=$ set ["new-york.jpg"; "san-diego.jpg"; "boston.jpg"; "los-angeles.jpg"]);
    mv_ "boston.jpg"      "music/";
    mv_ "new-york.jpg"    "music/";
    mv_ "los-angeles.jpg" "movie/";
    assert(ls_() $=$ set ["san-diego.jpg"; "art/"]);
    cd_ "art";
    assert(ls_() $=$ set ["movie/"; "music/"]);

    (*--------------------------------------------------------------*)
    cd_ "/USA&music";
    mkfile_ "chicago.jpg" "";
    cd_ "/";
    assert(ls_() $=$ set ["art/"; "port/"; "USA/"; "seaside/"; "hot/"]);
    cd_ "art";
    assert(ls_() $=$ set ["music/"; "movie/"; "port/"; "seaside/"]);

    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "geography";
    mvdir_ "seaside" "geography/";
    mvdir_ "port"    "geography/";
    mvdir_ "USA"     "geography/";
    mvdir_ "capital" "geography/";

    (*--------------------------------------------------------------*)
    (* \subsubsection{Organise properties automatically} *)
    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "type:";
    cd_ "/type:picture"; mkfile_ "misc.jpg" "";
    cd_ "/type:program"; mkfile_ "foo.c" "";
    cd_ "/";
    assert(ls_() $=$ set ["type:/"; "art/"; "geography/"; "hot/"]);
    cd_ "type:";
    assert(ls_() $=$ set ["type:program/"; "type:picture/"]);

    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "plugins";
    cd_ "plugins";
    mkdir "logic:";
    cd_ "/";
    mkdir "year:";
    cd_ "logic:year:";
    ignore(mkfile "int_logic" "" (Some (Logic (wrap_logic interval_logic is_formula_int))));
    cd_ "/year:2000"; mkfile_ "vacation-corsica.jpg" "";
    cd_ "/year:2001"; mkfile_ "vacation-england.jpg" "";
    cd_ "/year:2002"; mkfile_ "vacation-france.jpg" "";
    cd_ "/year:>2000";
    assert(ls_() $=$ set ["year:2001/"; "year:2002/"]);
    cd_ "/year:";
    assert(ls_() $=$ set ["year:2000/"; "year:>2000/"]);

    (*--------------------------------------------------------------*)
    assert(interval_logic (Prop "2001")  (Prop ">2000") = true);
    assert(interval_logic (Prop ">2000") (Prop "2001")  = false);

    (*--------------------------------------------------------------*)
    (* \subsubsection{Organise files automatically} *)
    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "size:";
    mkdir "name:";
    mkdir "ext:";
    mkdir "mypictures";
    cd_ "mypictures";
    mkfile_ "big.jpg" "xxxxxxxxxxxxxxxxx";
    mkfile_ "small.jpg" "xx";
    cd_ "ext:jpg";
    assert(ls_() $=$ set [
       (* update: dont see anymore the size:xxx cos here both will have
        * just size:0ko => dont be displayed
        *)
       "size:2/"; "size:17/";
       "name:small/"; "name:big/"]);
    (*--------------------------------------------------------------*)
    cd_ "/plugins";
    mkdir "transducer:";
    cd_ "/transducer:mp3";
    ignore(mkfile "mp3_transducer" "" (Some (Trans (wrap_transducer mp3_transducer))));
    cd_ "/";
    mkdir "mymusic";
    cd_ "mymusic";
    mkdir "artist:";
    mkdir "genre:";
    mkfile_ "staying_alive.mp3" "tag_genre=Disco\ntag_artist=BeeGees\ndataaaa";
    mkfile_ "ete_indien.mp3"    "tag_genre=Pop\ntag_artist=JoeDassin\ndata";
    assert(ls_() $=$ set [ "genre:Disco/"; "genre:Pop/";
			   "artist:BeeGees/"; "artist:JoeDassin/";
			   "name:ete_indien/";  "name:staying_alive/";
                           "size:39/"; "size:42/"
			   (* update: no more cos now size:0Ko,
			   *)
			 ] );

    (*--------------------------------------------------------------*)
    cd_ "genre:Disco";
    assert(mp3_transducer (read "staying_alive.mp3") $=$
	   set [Prop "artist:BeeGees"; Prop "genre:Disco"]);

    (*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
    (* \subsection{On file contents} *)
    (*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

    (*--------------------------------------------------------------*)
    (* \subsubsection{Organise file contents} *)
    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "myprograms";
    cd_ "myprograms";
    mkfile_ "foo.c" (unlines
		       [ "int f(int x) {";
			 "int y;";
			 "assert(x > 1);";
			 "y = x;";
			 "fprintf(stderr, \"x = %d\", x);";
			 "return y * 2";
			 "}";
			 "int f2(int z) {";
			 "return z * 4";
			 "}";
		       ]);
    assert(c_adv_transducer (read "foo.c" +> lines_with_nl +> take 2)
	     = [ [Prop "var:x"; Prop "synchro"; Prop "function:f"];  (* ADDON *)
		 [Prop "var:y"; Prop "function:f"]]);

    (*--------------------------------------------------------------*)
    cd_ "/";
    mkdir "function:";
    mkdir "var:";
    mkdir "debugging";
    mkdir "error";
    mkdir "synchro"; (* ADDON *)
    cd_ "/plugins";
    mkdir "adv_transducer:";
    cd_ "/adv_transducer:c";
    ignore(mkfile "c_adv_transducer" "" (Some (AdvTrans (c_adv_transducer))));
    cd_ "/myprograms";
    assert(ls_() $=$ set ["foo.c"]);


    cd_ "parts";

    (*--------------------------------------------------------------*)
    assert(ls_() $=$ set
	     [ "debugging/"; "error/";
	       "function:f/"; "function:f2/";
	       "var:/";
	       "synchro/"; (* ADDON *)
	       "foo.c"
	     ] );
    assert(read "foo.c" +> lines_with_nl +> length = 10);

    (*--------------------------------------------------------------*)
    (* \subsubsection{Retrieve and select file contents} *)
    (*--------------------------------------------------------------*)
    cd_ "function:f/var:/";
    assert(ls_() $=$ set
	     [ "var:x/";  "var:y/";
	       "debugging/"; "error/";
	       "synchro/"; (* ADDON *)
	       "foo.c"]);
    assert(read "foo.c" +> lines_with_nl +> length  = 7);

    (*--------------------------------------------------------------*)
    cd_ "!(debugging|error)";
    assert(ls_() $=$ set ["var:x/"; "var:y/";
			   "synchro/"; (* ADDON *)
			   "foo.c"]);
    assert(read "foo.c" = unlines
	     [ "int f(int x) {";
	       "int y;";
	       ".........:1";
	       "y = x;";
	       ".........:2";
	       "return y * 2";
	       ".........:3";
	     ]);

    (*--------------------------------------------------------------*)
    (* \subsubsection{Manipulate file contents} *)
    (*--------------------------------------------------------------*)
    read "foo.c" +> Str.global_replace (Str.regexp "y") "z" +> write (Left "foo.c");
    assert(ls_() $=$ set ["var:x/"; "var:z/";
			   "synchro/"; (* ADDON *)
			   "foo.c"]);

    (*--------------------------------------------------------------*)
    cd_ "/myprograms/parts";
    assert(read "foo.c" = unlines
	     [ "int f(int x) {";
	       "int z;";
	       "assert(x > 1);";
	       "z = x;";
	       "fprintf(stderr, \"x = %d\", x);";
	       "return z * 2";
	       "}";
	       "int f2(int z) {";
	       "return z * 4";
	       "}";
	     ]);




  end

(*****************************************************************************)
let basicenv_demo () =
  begin

    mkdir "plugins";
    cd_ "plugins";
    mkdir "logic:";
    mkdir "transducer:";
    mkdir "adv_transducer:";

    cd_ "/";

    mkdir "synchro";
    mkdir "multisynchro";
    cd_ "multisynchro";
    mkdir "synchro1:";
    mkdir "synchro2:";
    mkdir "synchro3:";
    mkdir "synchro4:";
    mkdir "synchro5:";
    cd_ "/";


    mkdir "size:";
    mkdir "name:";
    mkdir "ext:";
    mkdir "date:";
(*     mkdir "file:"; *)

    mkdir "contain:";
    mkdir "paracontain:";


  end

(*****************************************************************************)
let logic_demo () =
  begin
    basicenv_demo ();

    mkdir "prop:";

    cd_ "/logic:prop:";
    ignore(mkfile "prop_logic" "" (Some (Logic (wrap_logic prop_logic is_formula_prop))));
(*    ignore(mkfile "prop_logic" "" (Some (Logic (uninteract_logic "p_logic/prop_logic")))); *)

    cd_ "/";
    mkdir "mine";
    cd_ "/mine/prop:a";
    mkfile_ "fa" "";
    cd_ "/mine/prop:b";
    mkfile_ "fb" "";
    cd_ "/mine/prop:a AND b/";
    mkfile_ "fab" "";
    cd_ "/mine/prop:a OR b/";

    cd_ "/mine/prop:a";
    ls_print(); (* assert dont see a AND b *)
    pr2 "";

    cd_ "/mine/prop:b";
    ls_print(); (* assert dont see a AND b *)

  end

(*****************************************************************************)
let relation_demo () =
  (*  persons (objs = persons, relation = mother/father/...)
   *  for  toones
   *  for  gods
   *
   *  music   (objs = songs, artists, genre?;      relation = author)
   *  webpage (objs = webpage,                     relation = cited)
   *
   *  person,  male/female,  mother/father (constraints with female/male),
   *   parents
   *  brother (constraints too),  husband/spouse
   *  sibling, uncle, ants, ...
   *  constraints/integreiry/semantic-checking/...
   *)

  let ln_ = Lfs.ln in
  (*  let cdbis *)

  begin
    basicenv_demo ();

    mkdir "person";

    mkdir "male"; mkdir "female";
    mkdir "tall"; mkdir "small";

    mkdir "father:";
    mkdir "mother:";

    cd_ "/person/male/";
     mkfile_ "pad" "";
     mkfile_ "alain" "";
     mkfile_ "wilfried" "";
     mkfile_ "maxence" "";


    cd_ "/person/female/";
     mkfile_ "mado" "";
     mkfile_ "dedef" "";
     mkfile_ "severine" "";
     mkfile_ "noemie" "";

    cd_ "/.ca/";
    ls_print(); print_endline "";

    cd_ "/.ext";
    ln_ "alain" "<father:of>" "pad";
    ln_ "alain" "<father:of>" "dedef";
    ln_ "alain" "<father:of>" "severine";

    ln_ "mado" "<mother:of>" "pad";
    ln_ "mado" "<mother:of>" "dedef";
    ln_ "mado" "<mother:of>" "severine";

    ln_ "wilfried" "<father:of>" "maxence";
    ln_ "wilfried" "<father:of>" "noemie";

    ln_ "severine" "<mother:of>" "maxence";
    ln_ "severine" "<mother:of>" "noemie";

    mv_ "alain" "small";
    mv_ "wilfried" "tall";

    cd_ "/";
    ls_print(); print_endline "";

    cd_ "/father:";
    ls_print(); print_endline "";

    cd_ "/father:is==wilfried";
    pr ">computing";
    ls_print(); print_endline "";
    pr "<end computing";

    cd_ "/father:is===(tall&tall&name:wilfried)";
    pr ">computing";
    ls_print(); print_endline "";
    pr "<end computing";

    cd_ "/father:/father:is=>=";
    pr ">computing";
    ls_print(); print_endline "";
    pr "<end computing";

  end

(*****************************************************************************)
let bibtex_demo () =
  begin
    basicenv_demo ();

    mkdir "author:";
    mkdir "domain:";
    mkdir "year:";
    mkdir "institution:";
    mkdir "title:";
    mkdir "typeref:";
    mkdir "ref:";

    cd_ "/adv_transducer:bib";
    ignore(mkfile "bibtex_adv_transducer" "" (Some (AdvTrans (bibtex_adv_transducer))));
(*     ignore(mkfile "generic_adv_transducer" "" (Some (AdvTrans (bibtex_adv_transducer)))); *)

    cd_ "/";
    mkdir "mine";
    cd_ "mine";
    mkfile_ "ridoux.bib" (Common.read_file "../data/pof/bibtex/ridoux.bib");
    ls_print();

    cd_ "parts/.relaxed/year:1988/author:/";
    ls_print();

end let res () =  begin

    cd_ "year:";
    ignore(read "ridoux.bib");
    cd_ "year:2002";
    print_endline (read "ridoux.bib");

    ls_print();
    read "ridoux.bib" +> Str.global_replace (Str.regexp "2002") "2001" +> write (Left "ridoux.bib");

    ls_print();
    print_endline (read "ridoux.bib");

    ignore(ls_());
    read "ridoux.bib" +> Common.lines_with_nl +> bibtex_adv_transducer

  end

(*****************************************************************************)
(* put in lfs source:
 * let mark_string mark = ("                    (*...:" ^ (i_to_s mark) ^ "*)")
 *)
let ml_demo () =
  begin
    basicenv_demo ();

    mkdir "error";
    mkdir "example";

    mkdir "myprograms";

    cd_ "/adv_transducer:ml";
    ignore(mkfile "ml_adv_transducer" "" (Some (AdvTrans (ml_adv_transducer))));

    cd_ "/myprograms";
    mkfile_ "lfs.ml" (read_file "lfs.ml");
    cd_ "parts";
    cd_ "!(error|example)";
    write_file "gen.ml" (read "lfs.ml");
  end

(*****************************************************************************)
let latex_demo () =
  begin
    basicenv_demo ();

    mkdir "chapter:";
    mkdir "section:";
    mkdir "subsection:";
    mkdir "title:";

    mkdir "comment:";
    mkdir "note:";
    mkdir "aspect:";

    mkdir "latex";
    mkdir "environment:";

    cd_ "/adv_transducer:tex";
(*     ignore(mkfile "late_adv_transducer" "" (Some (AdvTrans (c_adv_transducer)))); *)
(* 	     (Some (AdvTrans (uninteract_adv_transducer "/home/pad/work/lfs/code/p_adv_transducer/latex_adv_transducer.pl" Lfs.check_and_add_property)))); *)

    cd_ "/";
    mkdir "mine";
    cd_ "mine";
    mkfile_ "main.tex" (read_file "/home/pad/work/lfs/thesis/small.tex");
    ls_print();
    cd_ "parts";
    ls_print();
    pr (read "main.tex");

  end
(*****************************************************************************)
let constitution_demo () =
  begin
    basicenv_demo ();

    mkdir "title:";
    mkdir "section:";

    cd_ "/logic:section:";
(*     ignore(mkfile "string_logic" "" (Some (Logic (uninteract_logic "p_logic/string_logic2.pl")))); *)

    cd_ "/adv_transducer:constitution";
(*     ignore(mkfile "const_adv_transducer" "" (Some (AdvTrans (uninteract_adv_transducer  "p_adv_transducer/constitution_adv_transducer.pl" check_and_add_property)))); *)
(*     ignore(mkfile "generic_adv_transducer" "" (Some (AdvTrans (uninteract_adv_transducer  "p_adv_transducer/generic_adv_transducer.pl" check_and_add_property)))); *)

    cd_ "/";
    mkdir "mine";
    cd_ "mine";
(*     cd_ "*libert/"; *)
    mkfile_ "const.constitution" (read_file "../data/pof/constitution-europeene.constitution");
    ls_print();

    cd_ "parts/.best/";
    ls_print();


    cd_ "section:I-.*/";
    cd_ "section:II-.*/";
    cd_ "section:III-.*/";
    cd_ "section:IV-.*/";

  end


(*****************************************************************************)
(*
   cd_ "/";
   cd_ "google:Geees";
   ls_print();
*)

(*****************************************************************************)
let demo_bug_mix_beck () =
  begin
    mkdir "a";
    mkdir "b";
    mkdir "c";
    cd_ "/";
    cd_ "/a/b/";
    mkfile_ "ab" "empty";
    let ctx = Lfs.context !w in
    let allo = Lfs.ext (Single (Prop "true")) ctx in
    pr2_gen allo;
    cd_ "/";
    cd_ "/b/c/";
    mkfile_ "bc" "empty";
    let ctx = Lfs.context !w in
    let allo = Lfs.ext (Single (Prop "true")) ctx in
    pr2_gen allo;
    (* alt: mv a/b/ab . *)
    cd_ "/";
    cd_ "a/b/";
    mv_ "ab" "/";

    cd_ "/";
    rmdir "b";
    (* rmdir "a"; (* opt2 *) *)
    ls_print(); pr " done1";
    (* => ab  c/ *)
    rmdir "c";
    ls_print(); pr " done2";
    (* => ab  bc *)
    rm (Left "bc") +> ignore;
    let ctx = Lfs.context !w in
    let allo = Lfs.ext (Single (Prop "true")) ctx in
    pr2_gen allo;
    (* Lfs.check_world !Lfs.w; *)
    ls_print(); pr " done3";
    (* ab *)
    rm (Left "ab") +> ignore;
    ls_print(); pr " done4";

  end

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let statistics = ref false

let default_demo = "principles"
let demos_list =
  [
    "principles"   , principles_demo;
    "basic"        , basicenv_demo;

    (* the following first calls basicenv_demo *)

    "logic"        , logic_demo;
    "relation"     , relation_demo;

    "bibtex"       , bibtex_demo;
    "ml"           , ml_demo;
    "latex"        , latex_demo;
    "constitution" , constitution_demo;

    "bug_mix_beck" , demo_bug_mix_beck;
  ]
let all_demos_str = demos_list +> List.map fst +> Common.join " "

let demo = ref default_demo

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let main ()  =
  begin
    Lfs.copyright ();

    Common.verbose_level := 1;
    (* ugly: to have same value than real_lfs for file identifier *)
    Common._counter := 2;

    let options = [
      "-demo", Arg.Set_string demo,
      "<string> possible demos are: " ^ all_demos_str;
      "-stat", Arg.Set statistics,
      " ";
    ]
    in
    let usage_msg =
      ("Usage: " ^ Common.basename Sys.argv.(0) ^
          " [options]\nOptions are:")
    in
    let args = Common.parse_options options usage_msg Sys.argv in

    (match (args) with
    | [] ->
        (* !! the main entry !! *)
        Lfs.make_default_world ();
        let f = List.assoc !demo demos_list in
        f();
    | _ -> Arg.usage options usage_msg
    );

    if !statistics then begin
      Gc.full_major();
      Gc.compact();
      Common.memory_stat() +> print_endline;
      Lfs.stat_world !w;
    end;
    pr2 "Done";
  end

(*****************************************************************************)
let _ =
  if not !Sys.interactive then main ()
