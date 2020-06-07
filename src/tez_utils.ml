[@@@SCaml]
open SCaml

(* XXX confusing names; they handle mutez, not tez *)

let tez_to_nat t = t /$ Tz 0.000001
let nat_to_tez n = Tz 0.000001 *$ n
