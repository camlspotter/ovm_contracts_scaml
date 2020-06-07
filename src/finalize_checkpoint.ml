[@@@SCaml]
open SCaml
open Ovm_primitive_types
open Ovm_storage_types
open Ovm_global_types
open Ovm_event_types

type finalize_checkpoint_params = {
  token_type: token_type;
  checkpoint_property: property;
}

let finalize_checkpoint_action
    (finalize_checkpoint_params: finalize_checkpoint_params)
    (s: ovm_storage)
  : context =


  (* // TODO: check adjudication.isDecided(checkpoint) *)
  let checkpoint = {
    subrange = Primitive_coder.decode_range
        (Option.get (Map.get (Nat 0) finalize_checkpoint_params.checkpoint_property.inputs));
    state_update = Primitive_coder.decode_property
        (Option.get (Map.get (Nat 1) finalize_checkpoint_params.checkpoint_property.inputs));
    }
  in

  (* // Store checkpoint to storage *)
  let s = Store_checkpoint.store_checkpoint s finalize_checkpoint_params.token_type checkpoint in

  (* // Emit event *)
  let checkpoint_finalized_event: event_params = [
    Obj.pack finalize_checkpoint_params.token_type;
    Obj.pack (Get_checkpoint_id.get_checkpoint_id checkpoint);
    Obj.pack checkpoint;
  ] in
  let s = {s with
           events_storage = Emit_event.emit_event s.events_storage "CheckpointFinalized" checkpoint_finalized_event;
          }
  in
  (([] : ops), s)
