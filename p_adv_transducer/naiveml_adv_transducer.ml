open Common

open Common_adv_transducer

let ml_adv_transducer = fun xs -> 
  let _state = ref None in
  xs +> map (fun s -> 
    (if s =~ ".*assert"  then set [Prop "error"] else empty_list) $+$
    (if s =~ ".*example"  then set [Prop "example"] else empty_list) $+$
    set [Prop "true"]
  )
