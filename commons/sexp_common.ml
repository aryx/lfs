(* automatically generated by ocamltarzan *)

open Common

let sexp_of_either _of_a _of_b =
  function
  | Left v1 -> let v1 = _of_a v1 in Sexp.List [ Sexp.Atom "Left"; v1 ]
  | Right v1 -> let v1 = _of_b v1 in Sexp.List [ Sexp.Atom "Right"; v1 ]

let sexp_of_either3 _of_a _of_b _of_c =
  function
  | Left3 v1 -> let v1 = _of_a v1 in Sexp.List [ Sexp.Atom "Left3"; v1 ]
  | Middle3 v1 -> let v1 = _of_b v1 in Sexp.List [ Sexp.Atom "Middle3"; v1 ]
  | Right3 v1 -> let v1 = _of_c v1 in Sexp.List [ Sexp.Atom "Right3"; v1 ]


let sexp_of_filename v = Conv.sexp_of_string v
let sexp_of_dirname v = Conv.sexp_of_string v

let sexp_of_set _of_a = Conv.sexp_of_list _of_a

let sexp_of_assoc _of_a _of_b =
  Conv.sexp_of_list
    (fun (v1, v2) ->
       let v1 = _of_a v1 and v2 = _of_b v2 in Sexp.List [ v1; v2 ])

let sexp_of_hashset _of_a = Conv.sexp_of_hashtbl _of_a Conv.sexp_of_bool

let sexp_of_stack _of_a = Conv.sexp_of_list _of_a

let sexp_of_parse_info {
                         str = v_str;
                         charpos = v_charpos;
                         line = v_line;
                         column = v_column;
                         file = v_file
                       } =
  let bnds = [] in
  let arg = sexp_of_filename v_file in
  let bnd = Sexp.List [ Sexp.Atom "file"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_column in
  let bnd = Sexp.List [ Sexp.Atom "column"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_line in
  let bnd = Sexp.List [ Sexp.Atom "line"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_charpos in
  let bnd = Sexp.List [ Sexp.Atom "charpos"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_string v_str in
  let bnd = Sexp.List [ Sexp.Atom "str"; arg ] in
  let bnds = bnd :: bnds in Sexp.List bnds



let sexp_of_score_result =
  function
  | Ok -> Sexp.Atom "Ok"
  | Pb v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "Pb"; v1 ]

let sexp_of_score v =
  Conv.sexp_of_hashtbl Conv.sexp_of_string sexp_of_score_result v

let sexp_of_score_list v =
  Conv.sexp_of_list
    (fun (v1, v2) ->
       let v1 = Conv.sexp_of_string v1
       and v2 = sexp_of_score_result v2
       in Sexp.List [ v1; v2 ])
    v
