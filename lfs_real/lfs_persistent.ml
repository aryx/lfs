type database = {
    final:  unit -> unit;

    commit: unit -> unit;
    abort:  unit -> unit;
    checkpoint: unit -> unit;
    archives: unit -> string list;
  }

