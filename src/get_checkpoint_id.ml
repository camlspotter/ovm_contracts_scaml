[@@@SCaml]
open SCaml
open Ovm_primitive_types

let get_checkpoint_id (checkpoint: checkpoint) =
  Crypto.sha256 (Obj.pack checkpoint)
