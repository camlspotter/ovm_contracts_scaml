[@@@SCaml]
open SCaml
open Ovm_primitive_types
type checkpoint_id = bytes
type claim_id = string
type checkpoints = (checkpoint_id, checkpoint) map
type commitments = (nat, bytes) map
