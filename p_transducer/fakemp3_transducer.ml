open Common
open Common_transducer

(* fake mp3 transducer, just to test *)
let mp3_transducer = fun s ->
  set
    [ Prop ("genre:"  ^ (regexp_match s "tag_genre=\\(.+\\)"));
      Prop ("artist:" ^ (regexp_match s ".*\ntag_artist=\\(.+\\)"))
    ]

let _ = interact_transducer mp3_transducer
