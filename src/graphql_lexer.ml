open Result

open Source_pos

let map_some f o = match o with
  | Some v -> Some (f v)
  | None -> None

type lexer = {
  source: string;
  length: int;
  mutable position: source_position;
  mutable has_reached_eof: bool;
}

type token =
  | Name of string
  | Int of int
  | Float of float
  | String of string
  | Exclamation_mark
  | Dollar
  | Paren_open
  | Paren_close
  | Bracket_open
  | Bracket_close
  | Curly_open
  | Curly_close
  | Ellipsis
  | Dot
  | Colon
  | Equals
  | At
  | Pipe
  | End_of_file

let string_of_token t = match t with
  | Name s -> s
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Exclamation_mark -> "!"
  | Dollar -> "$"
  | Paren_open -> "("
  | Paren_close -> ")"
  | Bracket_open -> "["
  | Bracket_close -> "]"
  | Curly_open -> "{"
  | Curly_close -> "}"
  | Ellipsis -> "..."
  | Dot -> "."
  | Colon -> ":"
  | Equals -> "="
  | At -> "@"
  | Pipe -> "|"
  | End_of_file -> "[EOF]"

type lexerError =
  | Unknown_character of char
  | Unexpected_character of char
  | Unterminated_string
  | Unknown_character_in_string of char
  | Unknown_escape_sequence of string
  | Unexpected_end_of_file
  | Invalid_number

let make source = {
  source = source;
  length = String.length source;
  position = origin;
  has_reached_eof = false;
}

let peek_char lexer =
  if lexer.position.index >= lexer.length then
    None
  else
    Some (lexer.position.index, lexer.source.[lexer.position.index])

let peek_char_only lexer =
  match peek_char lexer with
  | Some (_, ch) -> Some ch
  | None -> None

let next_char lexer =
  let next = peek_char lexer in
  let () = match peek_char_only lexer with
    | Some '\n' -> lexer.position <- advance_line lexer.position
    | Some _ -> lexer.position <- advance_col lexer.position
    | _ -> () in
  next

exception Internal_lexer_error

let emit_single_char lexer token =
  let start_pos = lexer.position in
  let _ = match next_char lexer with
    | Some next -> next
    | None -> raise Internal_lexer_error
  in
  single_width start_pos token


let rec scan_over_whitespace lexer =
  match peek_char_only lexer with
  | Some '\t' -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some ' '  -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some '\n' -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some '\r' -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some ','  -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some '#'  -> let _ = next_char lexer in scan_to_end_of_line lexer
  | _ -> ()
and scan_to_end_of_line lexer =
  match peek_char_only lexer with
  | Some '\n' -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some '\r' -> let _ = next_char lexer in scan_over_whitespace lexer
  | Some _ -> let _ = next_char lexer in scan_to_end_of_line lexer
  | None -> ()


let is_name_start c =
  c = '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

let is_digit c = (c >= '0' && c <= '9')

let is_name_cont c =
  is_name_start c || is_digit c

let is_number_start c = is_digit c || c = '-'

let scan_name lexer =
  let start_pos = lexer.position in
  match next_char lexer with
  | None -> Error (zero_width lexer.position Unexpected_end_of_file)
  | Some (start_idx, end_idx) ->
    let rec scan_loop end_idx =
      match peek_char lexer with
      | Some (idx, ch) when is_name_cont ch ->
        let _ = next_char lexer in
        scan_loop idx
      | Some _ -> end_idx
      | None -> end_idx
    in
    let endIdx = scan_loop start_idx in
    Ok (start_end
          start_pos
          lexer.position
          (Name (String.sub lexer.source start_idx (endIdx - start_idx + 1))))

let scan_ellipsis_or_dot lexer =
  let start_pos = lexer.position in
  let rec scan_loop i =
    if i = 0 then Ok (start_end start_pos lexer.position Ellipsis)
    else match peek_char lexer with
      | Some (_, '.') -> let _ = next_char lexer in scan_loop (i - 1)
      | Some (_, ch) when i = 2 -> Ok (start_end start_pos lexer.position Dot)
      | Some (_, ch) -> let _ = next_char lexer in Error (single_width lexer.position (Unexpected_character ch))
      | None -> Error (zero_width lexer.position Unexpected_end_of_file)
  in scan_loop 3

let scan_digits lexer =
  let start_pos = lexer.position in
  match peek_char lexer with 
  | None -> Error (zero_width start_pos Unexpected_end_of_file)
  | Some (start_idx, _) ->
    let rec scan_loop end_idx =
      match peek_char lexer with
      | Some (idx, ch) when is_digit ch -> let _ = next_char lexer in scan_loop idx
      | Some _ | None -> end_idx
    in
    let end_idx = scan_loop start_idx in
    try Ok (int_of_string (String.sub lexer.source start_idx (end_idx - start_idx + 1))) with
    | Failure "int_of_string" -> Error (start_end start_pos lexer.position Invalid_number)


let scan_integer_part lexer =
  let is_negative = match peek_char_only lexer with
    | Some '-' -> let _ = next_char lexer in Ok true
    | Some _ -> Ok false
    | None -> Error (zero_width lexer.position Unexpected_end_of_file)
  in
  match is_negative with
  | Error e -> Error e
  | Ok neg -> match scan_digits lexer with
    | Error e -> Error e
    | Ok num -> Ok (if neg then -1 * num else num)

let scan_number lexer =
  let start_pos = lexer.position in
  let build_number int_part frac_part exp_part =
    let mantissa = frac_part 
                   |> map_some float_of_int 
                   |> map_some (fun frac -> if frac > 0.0 then frac /. (10.0 ** (frac |> log10 |> floor)) else 0.0)
                   |> map_some (fun m -> if int_part < 0 then -1.0 *. m else m) in
    let exp = exp_part 
              |> map_some float_of_int
              |> map_some (fun e -> 10.0 ** e) in
    let num_token = match (mantissa, exp) with 
      | None, None -> Int int_part
      | None, Some exp -> Float ((float_of_int int_part) *. exp)
      | Some mantissa, None -> Float ((float_of_int int_part) +. mantissa)
      | Some mantissa, Some exp -> Float (((float_of_int int_part) +. mantissa) ** exp)
    in
    Ok (start_end start_pos lexer.position num_token)
  in
  let scan_exp_part int_part frac_part = match peek_char_only lexer with
    | Some 'e' | Some 'E' -> begin let _ = next_char lexer in match scan_integer_part lexer with
      | Error e -> Error e
      | Ok exp_part -> build_number int_part frac_part (Some exp_part)
      end
    | None | Some _ -> build_number int_part frac_part None
  in
  match scan_integer_part lexer with
  | Error e -> Error e
  | Ok int_part ->
    match peek_char_only lexer with
    | Some '.' -> begin let _ = next_char lexer in match scan_digits lexer with
      | Error e -> Error e
      | Ok digits -> scan_exp_part int_part (Some digits)
      end
    | Some _ | None -> scan_exp_part int_part None

let scan_string lexer =
  let start_pos = lexer.position in
  match next_char lexer with
  | None -> Error (zero_width start_pos Unexpected_end_of_file)
  | Some _ ->
    let rec scan_loop acc =
      match peek_char_only lexer with
      | None -> Error (zero_width lexer.position Unterminated_string)
      | Some '\n' | Some '\r' -> Error (single_width lexer.position Unterminated_string)
      | Some '"' -> let _ = next_char lexer in 
        Ok (start_end start_pos lexer.position (String acc))
      | Some '\\' -> begin let _ = next_char lexer in match peek_char_only lexer with
        | None -> Error (zero_width lexer.position Unterminated_string)
        | Some '"' -> let _ = next_char lexer in acc ^ "\"" |> scan_loop
        | Some '\\' -> let _ = next_char lexer in acc ^ "\\" |> scan_loop
        | Some '/' -> let _ = next_char lexer in acc ^ "/" |> scan_loop
        | Some 'b' -> let _ = next_char lexer in acc ^ "\010" |> scan_loop
        | Some 'f' -> let _ = next_char lexer in acc ^ "\014" |> scan_loop
        | Some 'n' -> let _ = next_char lexer in acc ^ "\n" |> scan_loop
        | Some 'r' -> let _ = next_char lexer in acc ^ "\r" |> scan_loop
        | Some 't' -> let _ = next_char lexer in acc ^ "\t" |> scan_loop
        | Some 'u' -> Error (single_width lexer.position 
                               (Unknown_escape_sequence "\\uXXXX"))
        | Some ch -> Error (single_width lexer.position
                              (Unknown_escape_sequence ("\\" ^ String.make 1 ch)))
        end
      | Some ch -> let _ = next_char lexer in acc ^ (String.make 1 ch) |> scan_loop
    in
    scan_loop "" 

let scan_single_token lexer =
  if lexer.has_reached_eof then
    None
  else begin
    let () = scan_over_whitespace lexer in
    Some (match peek_char_only lexer with
        | Some '!' -> Ok (emit_single_char lexer Exclamation_mark)
        | Some '$' -> Ok (emit_single_char lexer Dollar)
        | Some '(' -> Ok (emit_single_char lexer Paren_open)
        | Some ')' -> Ok (emit_single_char lexer Paren_close)
        | Some '[' -> Ok (emit_single_char lexer Bracket_open)
        | Some ']' -> Ok (emit_single_char lexer Bracket_close)
        | Some '{' -> Ok (emit_single_char lexer Curly_open)
        | Some '}' -> Ok (emit_single_char lexer Curly_close)
        | Some ':' -> Ok (emit_single_char lexer Colon)
        | Some '=' -> Ok (emit_single_char lexer Equals)
        | Some '@' -> Ok (emit_single_char lexer At)
        | Some '|' -> Ok (emit_single_char lexer Pipe)
        | Some '.' -> scan_ellipsis_or_dot lexer
        | Some '"' -> scan_string lexer
        | Some ch when is_number_start ch -> scan_number lexer
        | Some ch when is_name_start ch -> scan_name lexer
        | Some ch -> Error (single_width lexer.position (Unknown_character ch))
        | None -> let () = lexer.has_reached_eof <- true in
          Ok (zero_width lexer.position End_of_file)
      )
  end

let consume lexer =
  let rec consumer acc =
    match scan_single_token lexer with
    | Some (Ok t) -> consumer (t :: acc)
    | Some (Error e) -> Error e
    | None -> Ok acc
  in
  match consumer [] with
  | Ok l -> Ok (List.rev l)
  | Error e -> Error e
