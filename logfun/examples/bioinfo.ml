(**
This is an example of a logic definition from logic functors.
It is dedicated to bioinformatics data, and especially the representation of sequences.
*)

module ParamOpt : Opt.PARAM =
  struct
    let none = []  (** Wherever a value is optional, it can be replaced by nothing! *)
  end

module IntervParam = struct let verbose = true end

(** Sub-logic for interger-valued attributes. *)

module NamesInt : Atom.PARAM =
  struct
    let names = ["chr";"pos";"nt_length";"nb_atoms";"c";"h";"n";"o";"s"]
      (** Names of integer attributes. *)
  end
module AttrIntervInt = 
  Pair.Make 
   (Atom.Make(NamesInt))
   (Interv.Make(IntervParam)
     (Int.Make))

(** Sub-logic for float-valued attributes. *)

module NamesFloat : Atom.PARAM =
  struct
    let names = ["cai";"mol_weight";"theo_pI";"aliphatic_index";"hydro"]
      (** Names of float attributes. *)
  end
module AttrIntervFloat =
  Pair.Make 
   (Atom.Make(NamesFloat))
   (Interv.Make(IntervParam)
     (Float.Make))

(** Sub-logic for DNA sequences with IUPAC notation for nucleotide classes. *)

module NamesDNA : Atom.PARAM =
  struct
    let names = ["dna"]
      (** Name of the DNA sequence attribute. *)
  end
module ParamDNA : Motif.PARAM =
  struct
    let toks_begin = [Token.LT]  (** '<' denotes the sequence beginning. *)
    let toks_end = [Token.GT]  (** '>' denotes the sequence end. *)
    let toks_sep = [Token.Minus]  (** '-' denotes the separator between elements. *)
    let toks_any = [Token.Ident "n"]  (** 'n' is used to denote gaps. *)
    let feat_length = false
    let feat_dist = false
    let gen_floating = false
    let gen_len = false
    let gen_min_xs = 1
    let gen_link_cond is_begin (min,max) is_end = max=min
      (** Constraint on the size of gaps when learning. *)
  end
module AttrDNA =
  Pair.Make
   (Atom.Make(NamesDNA))
   (Opt.Make(ParamOpt)
      (Motif.Make(ParamDNA)
        (Nt.Make)))

(** Sub-logic for protein sequences. *)

module NamesSequence : Atom.PARAM =
  struct
    let names = ["seq"]
      (** Name of the protein sequence attribute. *)
  end
module ParamSequence : Motif.PARAM =
  struct
    let toks_begin = [Token.LT]
    let toks_end = [Token.GT]
    let toks_sep = [Token.Minus]
    let toks_any = [Token.Ident "x"]
    let feat_length = false
    let feat_dist = false
    let gen_floating = true
    let gen_len = false
    let gen_min_xs = 3
    let gen_link_cond is_begin (min,max) is_end = (max <= 10 & max-min <= 2)
  end
module ParamAa : Aa.PARAM =
  struct
    let gen_class_size = 2
      (** Constraint on the maximal size of amino-acids classes when learning. *)
  end
module AttrSequence =
  Pair.Make
   (Atom.Make(NamesSequence))
   (Opt.Make(ParamOpt)
      (Motif.Make(ParamSequence)
        (Aa.Make(ParamAa))))

(** Sub-logic for secondary structure sequences. *)

module NamesStruct : Atom.PARAM =
  struct
    let names = ["struc"]
      (** Name of secondary structure attribute. *)
  end
module ParamStruct : Motif.PARAM =
  struct
    let toks_begin = [Token.LT]
    let toks_end = [Token.GT]
    let toks_sep = [Token.Minus]
    let toks_any = [Token.Ident "x"]
    let feat_length = false
    let feat_dist = false
    let gen_floating = true
    let gen_len = false
    let gen_min_xs = 3
    let gen_link_cond is_begin (min,max) is_end = max-min <= 2
  end
module ParamAbcn : Abcn.PARAM =
  struct
    let gen_cond _ (min,max) = max/min <= 3
      (** Constraint on secondary structure elements when learning. *)
  end
module AttrStruct =
  Pair.Make
   (Atom.Make(NamesStruct))
   (Opt.Make(ParamOpt)
      (Motif.Make(ParamStruct)
        (Abcn.Make(ParamAbcn))))

(** Sub-logic of one-word string attributes. *)

module NamesOneWord : Atom.PARAM =
  struct
    let names = ["name";"gene";"classification";"gene_disruption";"sw"]
  end
module ToolsOneWord : Sentence.PARAM =
  struct
    let normalize s = s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';'.';';';'?';'!';':';'\'';'(';')';'@';'{';'}';'[';']']
    let words s = []
  end
module AttrOneWord =
  Pair.Make
   (Atom.Make(NamesOneWord))
   (Sentence.Make(ToolsOneWord))

(** Sub-logic of many-words strings. *)

module NamesListWords : Atom.PARAM =
  struct
    let names = ["synonyms";"fun_cats";"prot_classes"]
  end
module ToolsListWords : Sentence.PARAM =
  struct
    let normalize s = String.lowercase s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';'.';';';'?';'!';':';'\'';'(';')';'@';'{';'}';'[';']']
    let stopword s = String.length s <= 3
    let rec prefixes_ w pos =
      try 
        let pos' = String.index_from w pos '_' in
        (true,(true,String.sub w 0 pos',false)) :: prefixes_ w (pos'+1)
      with Not_found -> [(true,(true,w,false))]
    let words s = List.fold_right (fun w res -> prefixes_ w 0 @ res) (Syntax.split (normalize,separator,stopword) s) []
  end
module AttrListWords = 
  Pair.Make
   (Atom.Make(NamesListWords))
   (Sentence.Make(ToolsListWords))

(** Sub-logic for boolean and arbitrary string attributes. *)

module HasParam =
  struct
    let toks = []
  end
module NamesOptSentence : Attr.PARAM =
  struct
    let names = [] (* [] here means any attribute. *)
  end
module Tools : Sentence.PARAM =
  struct
    let normalize s = String.lowercase s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';'.';';';'?';'!';':';'\'';'(';')';'@';'{';'}';'[';']']
    let stopword s = String.length s <= 3 or List.mem s ["from";"with";"avec";"pour"]
    let words s = List.map (fun s' -> (true,(false,s',false))) (Syntax.split (normalize, separator, stopword) s)
  end
module AttrOptSentence =
  Has.Make(HasParam)
   (Pair.Make
     (Attr.Make(NamesOptSentence))
     (Opt.Make(ParamOpt)
       (Sentence.Make(Tools))))

(** Union of all previous sub-logics. *)

module Attrval =
  Sum.Make
   (AttrIntervInt)
   (Sum.Make
     (AttrIntervFloat)
     (Sum.Make
       (AttrDNA)
       (Sum.Make
         (AttrSequence)
         (Sum.Make
           (AttrStruct)
           (Sum.Make
             (AttrOneWord)
             (Sum.Make
               (AttrListWords)
               (AttrOptSentence)))))))

(** Finally, conjunctions of the various valued attributes. *)

module ConjParam =
  struct
    let top = true
    let sep = Some Token.Comma
    let lgg = false
  end
module Log =
  Conj.Make(ConjParam)
   (Attrval)


(** Example of formulas in this logic:

- c = 5878, h = 9475, o = 1775, n = 1627, s = 24, name is "YAL001c",
  gene is "TFC3", chr = 1, strand is "c", pos = 151167, nt_length = 3573, cai = 0.12, mw = 132108.1, atoms = 18779, theo_pI = 9.38, aliphatic_index = 88.09, hydro = -0.512,
  'mfc04_01_01: rRNA synthesis', 'mfc04_03_01: tRNA synthesis', synonyms is "FUN24, TSV115",
  seq is 'MVLTIYPDELVQIVSDKIASNKGKITLNQLWDISGKYFDLSDKKVKQFVLSCVILKKDIEVYCDGAITTKNVTDIIGDANHSYSVGITEDSLWTLLTGYTKKESTIGNSAFELLLEVAKSGEKGINTMDLAQVTGQDPRSVTGRIKKINHLLTSSQLIYKGHVVKQLKLKKFSHDGVDSNPYINIRDHLATIVEVVKRSKNGIRQIIDLKRELKFDKEKRLSKAFIAAIAWLDEKEYLKKVLVVSPKNPAIKIRCVKYVKDIPDSKGSPSFEYDSNSADEDSVSDSKAAFEDEDLVEGLDNFNATDLLQNQGLVMEEKEDAVKNEVLLNRFYPLQNQTYDIADKSGLKGISTMDVVNRITGKEFQRAFTKSSEYYLESVDKQKENTGGYRLFRIYDFEGKKKFFRLFTAQNFQKLTNAEDEISVPKGFDELGKSRTDLKTLNEDNFVALNNTVRFTTDSDGQDIFFWHGELKIPPNSKKTPNKNKRKRQVKNSTNASVAGNISNPKRIKLEQHVSTAQEPKSAEDSPSSNGGTVVKGKVVNFGGFSARSLRSLQRQRAILKVMNTIGGVAYLREQFYESVSKYMGSTTTLDKKTVRGDVDLMVESEKLGARTEPVSGRKIIFLPTVGEDAIQRYILKEKDSKKATFTDVIHDTEIYFFDQTEKNRFHRGKKSVERIRKFQNRQKNAKIKASDDAISKKSTSVNVSDGKIKRRDKKVSAGRTTVVVENTKEDKTVYHAGTKDGVQALIRAVVVTKSIKNEIMWDKITKLFPNNSLDNLKKKWTARRVRMGHSGWRAYVDKWKKMLVLAIKSEKISLRDVEELDLIKLLDIWTSFDEKEIKRPLFLYKNYEENRKKFTLVRDDTLTHSGNDLAMSSMIQREISSLKKTYTRKISASTKDLSKSQSDDYIRTVIRSILIESPSTTRNEIEALKNVGNESIDNVIMDMAKEKQIYLHGSKLECTDTLPDILENRGNYKDFGVAFQYRCKVNELLEAGNAIVINQEPSDISSWVLIDLISGELLNMDVIPMVRNVRPLTYTSRRFEIRTLTPPLIIYANSQTKLNTARKSAVKVPLGKPFSRLWVNGSGSIRPNIWKQVVTMVVNEIIFHPGITLSRLQSRCREVLSLHEISEICKWLLERQVLITTDFDGYWVNHNWYSIYEST',
  struc match cbbbbccaaaaaaaaaaaacccccbbbbaaaaaacccbbccccccbbbbbbbbbbccccbbbbbcccccccccbbbbbcccbbbbbbccccbbbbbbbbbcccccccaaaaaaaaaaaaccccccccababacccccccccaaaaaaaaaaacccbbbbcccbbbbbbbbcccccccccccbbbbcaaaaaaaaaaacccccabbbbaaaabbccccaaaaaaaaaaaaaaaccccccbbbbbbcccccbbbbbbbaaacccccccccccccccccccccccccccaacccccccccccccccccaaaaccccbbbcccccccccaaaaccccccccbbbbbbcccccbbbbaaaaaacccaaaccaaaacaaaaaaaacccccccccbbbbbbbbcccccbbbbbbbaaaaaacccccccccccccaaacccccccccbcccccbbbbbcbbbbbbcccccbbbbbbcccccccccccccccccabbbbcccccbbbbbccccccbbbbbbbcccccccccccccccccccbbbbbbbbbbbcccaaaaaaaaaaaaaaaaaaccccaaaaaaaaaaaaaaacccbbbbccbbbcccbbbbbbbaccccccccccccbbbbbccccaaaaaaaaacccccccbbbbbbbcccbbbbbbcccccccccccccaaaaaaaaacccccbbbbccaaacccccbbbbccccbbccccbbcccbbbbbbbbcccccbbbbcccccaaaaaabbbbbbbcccbbbbaaaaaaccccccccaaaaaabbbbbbbccccaaaaaaaaaaabbbbbbcccccbcccccccbbbbbbbccccccacccccbbbbccaaccccbbbbbbccccccccccaaaaaaaaaaaaaaaccbbbbbbbcccccccccccaaaaaabbbbbbbcccccaaaaaaaaacccccccaaaaaaacccbbbbbccbbbcccccaaaaacccccccccbbbbbbccaaaaaacccbbbbbcccccccbbbbbbbccccccccccccbbccccccbbbcbbbbbbbccccbbbbbccccccccccbbbbbbccccccbbbbbbcccccccaaaaaaaabbbbbbbbcccccaaaaaaaaaaaccaaaaaaaaaaaacccbbbbbccccbbbbcccbbbbbcc

- nt_length = 3573
- nt_length > 3000
- nt_length < 4000
- nt_length in 3000 .. 4000

- theo_pI = 9.38
- theo_pI > 9.
- theo_pI < 10.
- theo_pI in 9. .. 10.

- name is "YAL001c"
- name beginswith "YA"
- name endswith "c"
- name contains "L"

- 'mfc04_03_01: tRNA synthesis'

- seq match <M-x(2,3)-[AILPV]

- struc match c(3)-b(4,5)-c(2,4)-x(0,2)-b(4,5)-c(2,4)>

*)

(** Small program prompting the user to enter formulas, and answering whether this is syntactically correct. *)

let _ =
  try
    while true do
      print_endline "\nEnter a formula (or 'exit' to quit):";
      let line = read_line () in
      if line = "exit" then raise End_of_file;
      try
        match Syntax.from_string line with parser
        | [<_ = Log.parse; _ = Stream.empty>] -> print_endline "OK"
      with Stream.Error msg -> print_endline (if msg="" then "Syntax error" else msg)
    done
  with
  | End_of_file -> print_endline "Good bye"
  | e -> print_endline (Printexc.to_string e)
