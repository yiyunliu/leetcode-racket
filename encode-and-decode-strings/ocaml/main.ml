(* , is used as the separator *)
(* , itself is encoded as \, *)
(* \ itself is encoded as \\ *)
open OUnit2

let encode_char ch =
  match ch with
  | ',' -> ['\\';',']
  | '\\' -> ['\\';'\\']
  | _ -> [ch]

let encode_strings strs =
  let buf = Buffer.create 16 in

  let process_str =
    String.iter (fun ch ->
        encode_char ch |>
          List.iter (fun ch ->
              BatBuffer.add_char buf ch)) in

  let process_strs =
    List.iter
      begin
        fun str ->
        Buffer.add_char buf ',';
        process_str str
      end in

  begin
    match strs with
    | str :: strs ->
       process_str str;
       process_strs strs
    | [] -> ()
  end;
  Buffer.contents buf

let decode_strings str =
  let iter = BatString.enum str in

  let buf = Buffer.create 16 in

  let rec loop acc =
    match BatEnum.get iter with
    | None ->
       (Buffer.contents buf)::acc
    | Some ch ->
       match ch with
       | ',' ->
          let s = Buffer.contents buf in
          Buffer.clear buf;
          loop (s::acc)
       | '\\' ->
          begin
            match BatEnum.get iter with
            | None -> raise (Invalid_argument "unrecognized encoding")
            | Some ch0  ->
               match ch0 with
               | '\\' -> Buffer.add_char buf '\\';
                         loop acc
               | ',' -> Buffer.add_char buf ',';
                        loop acc
               | _ -> raise (Invalid_argument "unrecognized encoding")
          end
       | _ ->
          Buffer.add_char buf ch;
          loop acc in
  List.rev (loop [])

let test1 _ =
  let strs = ["neet";"code";"love";"you"] in
  assert_equal strs (strs |> encode_strings |> decode_strings)

let test2 _ =
  let strs = ["neet,";"code\\";"love,,";"you,,"] in
  assert_equal strs (strs |> encode_strings |> decode_strings)

let suite = "suite" >::: ["test1">::test1;"test2">::test2]
let _ = run_test_tt_main suite
