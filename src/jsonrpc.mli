type id = [ `Int of int | `Null | `String of string ]

type json = Yojson.Basic.json

type t =
  | Request of { name : string; params : json option; id : id; }
  | Notification of { name : string; params : json option; }
  | Response of { result : json; id : id; }
  | Error of { code : int; message : string; data : json option; id : id; }

val request : id:id -> ?params:json -> string -> json

val notification : ?params:json -> string -> json

val response : json -> id -> json

val error : int -> string -> json option -> id -> json

val to_json : t -> json

val of_json : json -> t

val of_string : string -> t

val to_string : t -> string

