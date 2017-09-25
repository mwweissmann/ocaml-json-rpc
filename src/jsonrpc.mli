type id = [ `Int of int | `Null | `String of string ]
(** identifier for request/response/error messages *)

type json = Yojson.Basic.json

type t =
  | Request of { name : string; params : json option; id : id; }
  | Notification of { name : string; params : json option; }
  | Response of { result : json; id : id; }
  | Error of { code : int; message : string; data : json option; id : id; }
(** type of a JSON-RPC 2.0 message *)

val request : id:id -> ?params:json -> string -> json
(** [request ~id ~params name] generates a request message with method name
    [name], identifier [id] and parameters [params] directly as JSON value.
    [request ~id ~params name = to_json (Request { name; params; id })] *)

val notification : ?params:json -> string -> json
(** [notification ~params name] generate a notification message with method
    name [name] and parameters [params] directly as JSON value.
    [notification ~params name = to_json (Notification { name; params }] *)

val response : id:id -> json -> json
(** [response ~id data] generates a response message with identifier [id] and
    payload [data] directly as JSON value.
    [response ~id result = to_json (Response { result; id })] *)

val error : id:id -> ?data:json -> int -> string -> json
(** [error ~id ~data code message] generates an error message with identifier
    [id] and error-triplet [code], [message] and optionally [data].
    [error ~id ~data code message = to_json (Error { code; message; data; id })] *)

val to_json : t -> json
(** convert a message type [t] to JSON *)

val to_string : t -> string
(** convert a message type [t] to string *)

val of_json : json -> (t, string) Result.result
(** parse a JSON value to a JSON-RPC message value; in case of an error,
    [Result.Error] is returned. *)

val of_json_exn : json -> t
(** parse a JSON value to a JSON-RPC message value; in case of an error,
    [Invalid_argument] is raised. *)

val of_string : string -> (t, string) Result.result
(** parse a JSON value to a JSON-RPC message value from a string; in case of an
    error, [Result.Error] is returned. *)

val of_string_exn : string -> t
(** parse a JSON value to a JSON-RPC message value from a string; in case of an
    error either [Invalid_argument] or [Yojson.Json_error] are raised. *)

module Server : sig
  type t
  (** A JSON-RPC server *)
 
  type reply = (json, int * string) Result.result
  (** the return value of an JSON-RPC function *)

  type callback = json option -> reply
  (** a JSON-RPC function *)

  module Error : sig
    val parse_error : reply
    (** The JSON-RPC error value for parse error *)

    val invalid_request : reply
    (** The JSON-RPC error value for invalid request *)

    val method_not_found : reply
    (** The JSON-RPC error value for method not found *)

    val invalid_params : reply
    (** The JSON-RPC error value for invalid parameters *)

    val internal_error : reply
    (** The JSON-RPC error value for internal error *)
  end

  val create : unit -> t
  (** create a JSON-RPC server; the default function will be set to
      [(fun _ -> Error.method_not_found)], returning this error message as
      reply to all requests. *)

  val add_cb : t -> name:string -> callback -> unit
  (** [add_cb srv ~name cb] add the callback function [cb] to the server [src].
      The function will be called in a JSON-RPC request matches the given
      function name [name]. *)

  val set_default : t -> callback -> unit
  (** [set_default src cb] sets the default callback function to [cb] in the
      server [srv]. This function is called if no callback function matches the
      function name in the RPC reuqest. *)
      
  val remove_cb : name:string -> t -> unit
  (** [remove_cb ~name srv] removes the callback function for name [name] from
      the server [srv]. *)

  val evalj : t -> json -> json option
  (** [evalj srv json_in] generates a return value for the given JSON-RPC
      request [json_in] for the server [srv]. If no answer is to be sent back
      to the caller, this function will return [None]. *)

  val eval : t -> string -> string option
  (** [eval srv s] works just like [evalj srv json] but will parse the value
      [s] to JSON; it also converts the reply to a string value to be sent back
      to the caller. *)
end

