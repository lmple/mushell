open Sys
open Unix
open Printf

let prompt =
  Printf.printf "mush> ";
  try
    Some(read_line())
  with End_of_file -> None

let is_supported_os =
  let os = os_type in
  match os with
  | "Unix" -> true
  | _ -> false

let rec shell () =
  begin
    let command = Array.of_list (Some(prompt) |> String.split_on_char ' ') in
    match fork() with
      | 0 -> begin
          try
            execvp (Array.get command 0) command;
          with _ -> printf "%s" "error while execv\n"; exit(-1)
        end
      | -1 -> printf "%s" "fork error";
      | _ -> ignore(wait());shell()
  end;;

let () =
  if not is_supported_os then
    begin
      print_endline "Unsupported OS";
    end
  else
    shell()
