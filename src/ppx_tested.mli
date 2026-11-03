open! Ppxlib

module For_testing : sig
  module Point = Point_collector.Point

  val rewrite_dev_structure : library:string -> structure -> structure

  (** The coverage points collected by the dev rewrite of [structure]. *)
  val dev_points : library:string -> structure -> Point.t list

  val rewrite_release_structure : structure -> structure
end
