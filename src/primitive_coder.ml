open SCaml
open Ovm_primitive_types
open Ovm_global_types

let encode_address (a : address) = Obj.pack a

let decode_address bytes : address option = Obj.unpack bytes

let encode_number (n: nat) = Obj.pack n

let decode_number bytes : nat option = Obj.unpack bytes

(* XXX We can directly pack unpack records *)
let encode_range (range: range) = Obj.pack (range.start_, range.end_)

(* XXX We can directly pack unpack records *)
let decode_range bytes : range =
  match (Obj.unpack bytes : (nat * nat) option) with
  | Some (start_, end_) -> {start_; end_}
  | None -> failwith "decode error"

(* XXX We can directly pack unpack records *)
let encode_property (property: property) =
  Obj.pack (property.predicate_address, property.inputs)

(* XXX We can directly pack unpack records *)
let decode_property bytes : property =
  match (Obj.unpack bytes : (address * (nat, bytes) map) option) with
  | Some (predicate_address, inputs) -> {predicate_address; inputs}
  | None -> failwith "decode error"

(* XXX _s is not used... *)
let pack_property action (_s: bytes) : (ops * bytes) =
  ([], encode_property action)
