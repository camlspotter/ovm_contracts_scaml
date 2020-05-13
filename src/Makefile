SCAMLC=scamlc

.PHONY:default
default: ovm.tz

clean:
	rm -f *.cm* *.tz

.SUFFIXES: .ml .mli .cmo .cmi .cmx .tz

MLS= \
	ovm_event_types.ml \
	ovm_primitive_types.ml \
	ovm_iterable_types.ml \
	ovm_storage_types.ml \
	ovm_global_types.ml \
	emit_event.ml \
	commitment.ml \
	primitive_coder.ml \
	get_checkpoint_id.ml \
	store_checkpoint.ml \
	extend_deposited_ranges.ml \
	deposit.ml \
	finalize_checkpoint.ml \
	tez_utils.ml \
	ovm.ml

ovm.tz: $(MLS)
	$(SCAMLC) $(MLS)
