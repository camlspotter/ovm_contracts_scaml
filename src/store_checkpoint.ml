[@@@SCaml]
open SCaml
open Ovm_primitive_types
open Ovm_storage_types

let store_checkpoint
    (s: ovm_storage)
    (token_type: address)
    (checkpoint: checkpoint)
    : ovm_storage =
  let deposit_storage = Option.get (Map.get token_type s.deposit_storages) in
  let checkpoint_id = Get_checkpoint_id.get_checkpoint_id checkpoint in
  let deposit_storage = 
    { deposit_storage with
      checkpoints = Map.update checkpoint_id (Some checkpoint) deposit_storage.checkpoints;
    }
  in
  { s with
    deposit_storages = Map.update token_type (Some deposit_storage) s.deposit_storages;
  }
