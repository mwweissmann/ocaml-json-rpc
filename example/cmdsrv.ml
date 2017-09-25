let _ =
  let server = Jsonrpc.Server.create () in

  let sub = function
    | Some (`List [`Int a; `Int b]) -> Result.Ok (`Int (a - b))
    | _ -> Jsonrpc.Server.Error.invalid_params
  in
  Jsonrpc.Server.add_cb server ~name:"subtract" sub;
  (* {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1} *)

  let sum = function
    | Some (`List xs) ->
      let s = List.fold_left (fun sum x -> match x with `Int i -> i + sum | _ -> sum) 0 xs in
      Result.Ok (`Int s)
    | _ -> Jsonrpc.Server.Error.invalid_params
  in
  Jsonrpc.Server.add_cb server ~name:"sum" sum;
  (* {"jsonrpc": "2.0", "method": "sum", "params": [21, 1, 19, 1], "id": 23} *)

  Jsonrpc.Server.add_cb server ~name:"yes" (fun _ -> Result.Ok (`Bool true));
  (* {"jsonrpc": "2.0", "method": "yes", "id": 42} *)

  Jsonrpc.Server.add_cb server ~name:"no" (fun _ -> Result.Ok (`Bool false));
  (* {"jsonrpc": "2.0", "method": "no", "params": ["I", "dont", "care", 19], "id": 17} *)

  let oneshot = (fun _ -> Jsonrpc.Server.remove_cb ~name:"oneshot" server; Result.Ok `Null) in
  Jsonrpc.Server.add_cb server ~name:"oneshot" oneshot;
  (* {"jsonrpc": "2.0", "method": "oneshot", "id":23} *)

  while true; do
    let line = read_line () in
    let result = Jsonrpc.Server.eval server line in
    print_endline (match result with | None -> "-" | Some x -> x)
  done

