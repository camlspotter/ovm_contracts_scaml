module Entrypoints = struct
  [@@@SCaml]
  let [@entry] finalize_checkpoint p s = Finalize_checkpoint.finalize_checkpoint_action p s
  let [@entry] submit p s = Commitment.submit_action p s

  let [@entry] deposit p s = Deposit.deposit_action p s
end

let () = SCamlc.Ppx.emit ~outputprefix:"ovm"
