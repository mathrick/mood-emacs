"Support for Perforce (AKA Helix Core) version control

There are two flavours of P4 integration:

- Plain, via p4.el, which adds functions to interact with the P4
  client directly

- Magit-P4, which integrates git-p4 into Magit, and allows
  interacting with P4 via local git clones. Requires Magit module
  to be enabled as well

Magit-P4 internally uses p4.el, therefore plain integration is
always enabled, but magit support can be disabled."

(defflag -magit "Disable magit-p4 support")
