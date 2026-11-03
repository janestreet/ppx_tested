(** [Rewrite_release] is the release-build expansion.

    This pass erases coverage instrumentation while still type-checking [%tested]
    payloads. *)

open! Ppxlib

val rewrite_structure : structure -> structure
