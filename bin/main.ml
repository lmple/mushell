open Sys
open Unix
open Printf

let prompt () =
  Printf.printf "mush> ";
  try
    let lines = Some(read_line()) in
    lines
  with End_of_file -> None

let is_supported_os =
  let os = os_type in
  match os with
  | "Unix" -> true
  | _ -> false

let rec shell () =
    let enteredCommand = match prompt() with
        | Some(c) -> c
        | None -> ""
    in
    let command = Array.of_list (enteredCommand |> String.split_on_char ' ') in
    match fork() with
      | 0 -> begin
          try
            execvp (Array.get command 0) command;
          with _ -> printf "%s" "error while execv\n"; exit(-1)
        end
      | -1 -> printf "%s" "fork error";
      | _ -> ignore(wait());shell()

let () =
  if not is_supported_os then
    begin
      print_endline "Unsupported OS";
    end
  else
    shell()
