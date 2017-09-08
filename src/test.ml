let _ =
  let rpc = Jsonrpc.notification ~params:(`List [`Int 1; `Int 2; `Int 3]) "add" in
  print_endline (Yojson.Basic.to_string rpc)

let _ =
  let json = Jsonrpc.request ~params:(`String "bar") "foo" ~id:(`Int 42) in
  print_endline (Yojson.Basic.to_string json);
  let rpc = Jsonrpc.to_json (Jsonrpc.of_json json) in
  print_endline (Yojson.Basic.to_string rpc)
  
let _ =
  let json = Jsonrpc.response (`Int 6) (`String "23") in
  print_endline (Yojson.Basic.to_string json);
  let rpc = Jsonrpc.to_json (Jsonrpc.of_json json) in
  print_endline (Yojson.Basic.to_string rpc)


