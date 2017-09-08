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

let response data id =
  `Assoc [
    "jsonrpc", `String "2.0";
    "result", data;
    "id", json_of_id id;
  ]

let error code message data id =
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
  | Response { result; id } -> response result id
  | Error { code; message; data; id } -> error code message data id

let assoc_safe k xs =
  try Some (List.assoc k xs) with Not_found -> None

let of_json : (json -> t) = function
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

let of_string s = of_json (Yojson.Basic.from_string s)

let to_string r = Yojson.Basic.to_string (to_json r)
