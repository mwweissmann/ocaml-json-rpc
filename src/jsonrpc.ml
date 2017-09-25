(*
--> {"jsonrpc": "2.0", "method": "subtract", "params": {"minuend": 42, "subtrahend": 23}, "id": 3}
<-- {"jsonrpc": "2.0", "result": 19, "id": 3}
*)

type id = [`Null | `String of string | `Int of int]

type json = Yojson.Basic.json

let id_of_json : (json -> id) = function
  | `Null as id -> id
  | `Int _ as id -> id
  | `String _ as id -> id
  | _ -> invalid_arg "invalid type for id"

let json_of_id : id -> json = Obj.magic

type t =
  | Request of { name : string; params : json option; id : id }
  | Notification of { name : string; params : json option }
  | Response of { result : json; id : id }
  | Error of { code : int; message : string; data : json option; id : id }

let request_ name id params =
  let jid = match id with
    | None -> []
    | Some i -> ["id", json_of_id i]
  in
  let jparams = match params with
    | None -> []
    | Some json -> ["params", json]
  in
  `Assoc ([
    "jsonrpc", `String "2.0";
    "method", `String name;
  ] @ jparams @ jid)

let request ~id ?params name = request_ name (Some id) params

let notification ?params name = request_ name None params

let response ~id data =
  `Assoc [
    "jsonrpc", `String "2.0";
    "result", data;
    "id", json_of_id id;
  ]

let error ~id ?data code message =
  let jdata = match data with
    | None -> []
    | Some json -> ["data", json]
  in
  `Assoc [
    "jsonrpc", `String "2.0";
    "error", (`Assoc (["code", `Int code; "message", `String message] @ jdata));
    "id", json_of_id id;
  ]

let to_json = function
  | Request { name; params; id } -> request_ name (Some id) params
  | Notification { name; params } -> request_ name None params
  | Response { result; id } -> response ~id result
  | Error { code; message; data; id } ->
    begin match data with
    | None -> error ~id code message
    | Some data -> error ~id ~data code message
    end

let assoc_safe k xs =
  try Some (List.assoc k xs) with Not_found -> None

let of_json_exn : (json -> t) = function
  | `Assoc xs ->
    begin match assoc_safe "jsonrpc" xs with
    | Some (`String x) when x <> "2.0" -> invalid_arg "protocol version mismatch"
    | Some (`String x) when x = "2.0" ->
      begin match assoc_safe "method" xs with
      | Some (`String name) ->
        let params = assoc_safe "params" xs in
        begin match assoc_safe "id" xs with
        | None -> Notification { name; params }
        | Some jid -> Request { name; params; id = id_of_json jid }
        end
      | Some _ -> invalid_arg "field name is not a string"
      | None ->
        begin match assoc_safe "id" xs with 
        | None -> invalid_arg "missing field id"
        | Some id ->
          let id = match id with | `Null -> `Null | `String x -> `String x | `Int i -> `Int i | _ -> invalid_arg "invalid type of id" in
          begin match assoc_safe "result" xs with 
          | Some result -> Response { result; id }
          | None ->
            begin match assoc_safe "error" xs with
            | None -> invalid_arg "missing field"
            | Some (`Assoc err) ->
              begin match assoc_safe "code" err, assoc_safe "message" err with
              | Some (`Int code), Some (`String message) -> Error { code; message; data = assoc_safe "data" err; id }
              | _ -> invalid_arg "missing field in error"
              end
            | Some _ -> invalid_arg "invalid type for field error"
            end
          end
        end
      end
    | None -> invalid_arg "missing field jsonrpc"
    | Some _ -> invalid_arg "field jsonrpc is not a string"
    end
  | _ -> invalid_arg "not an associative array"

let ewrap f x =
  try Result.Ok (f x) with
  | Invalid_argument e -> Result.Error e
  | Yojson.Json_error e -> Result.Error e

let of_json = ewrap of_json_exn

let of_string_exn s = of_json_exn (Yojson.Basic.from_string s)

let of_string = ewrap of_string_exn

let to_string r = Yojson.Basic.to_string (to_json r)

module Server = struct
  type reply = (json, int * string) Result.result

  type callback = json option -> reply

  type t = {
    callbacks : (string, callback) Hashtbl.t;
    mutable default : callback
  }

  module Error = struct
    let parse_error_v = (-32700, "parse error")
    let parse_error = Result.Error parse_error_v
    let invalid_request = Result.Error (-32600, "invalid request")
    let method_not_found = Result.Error (-32601, "method not found") 
    let invalid_params = Result.Error (-32602, "invalid params")
    let internal_error = Result.Error (-32603, "internal error")
  end

  let create () = {
    callbacks = Hashtbl.create 64;
    default = (fun _ -> Error.method_not_found);
  }

  let add_cb srv ~name cb = Hashtbl.add srv.callbacks name cb

  let remove_cb ~name srv = Hashtbl.remove srv.callbacks name

  let set_default srv cb = srv.default <- cb

  let find srv k =
    try
      Hashtbl.find srv.callbacks k
    with Not_found -> srv.default

  let evalj srv js =
    match of_json js with
    | Result.Error _ -> None
    | Result.Ok (Request { name; params; id }) ->
      begin match (find srv name) params with
      | Result.Ok result-> Some (response ~id result)
      | Result.Error (code, e) -> Some (error ~id ~data:js code e)
      end
    | Result.Ok (Notification { name; params }) ->
      begin match (find srv name) params with
      | Result.Ok result-> None
      | Result.Error (code, e) -> None
      end
    | Result.Ok _ -> None

  let eval srv s =
    try
      begin match evalj srv (Yojson.Basic.from_string s) with
      | None -> None
      | Some js -> Some (Yojson.Basic.to_string js)
      end
    with Yojson.Json_error _ ->
      let err = error ~id:`Null (fst Error.parse_error_v) (snd Error.parse_error_v) in
      Some (Yojson.Basic.to_string err)
end

