let pp_strings iter ppf list =
  iter (fun s ->
             Format.fprintf ppf "%s@ " s) list

let pp_slist ppf list =
  List.iter (fun s ->
             Format.fprintf ppf "%s@ " s) list

let pp_seq ?brackets:(brackets=false) ppf pp list =
  if (brackets) then
    (* "@[[]@]","@[[%a]@]","@[[@]@[<v>%a@ ","%a@ ","@]@[]@]" *)
    match list with
    | [] -> Format.fprintf ppf "@[[]@]"
    | x::[] -> Format.fprintf ppf "@[[%a]@]" pp x
    | x::xs ->
      Format.fprintf ppf "@[[@]@[<v>%a@ " pp x;
      List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
      Format.fprintf ppf "@]@[]@]"
  else
    match list with
    (* "@[@]","@[%a@]","@[<v>%a@ ","%a@ ","@]" *)
    | [] -> Format.fprintf ppf "@[@]"
    | x::[] -> Format.fprintf ppf "@[%a@]" pp x
    | x::xs ->
      Format.fprintf ppf "@[<v>%a@ " pp x;
      List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
      Format.fprintf ppf "@]"

let pp_h ?brackets:(brackets=false) ppf pp list =
  if (brackets) then
    match list with
    | [] -> Format.fprintf ppf "@[[]@]"
    | x::[] -> Format.fprintf ppf "@[[%a]@]" pp x
    | x::xs ->
      Format.fprintf ppf "@[[@]@[<h>%a@ " pp x;
      List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
      Format.fprintf ppf "@]@[]@]"
  else
    match list with
    | [] -> Format.fprintf ppf "@[@]"
    | x::[] -> Format.fprintf ppf "@[%a@]" pp x
    | x::xs ->
      Format.fprintf ppf "@[<h>%a@ " pp x;
      List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
      Format.fprintf ppf "@]"

(* let pp_header ppf  *)

let pp_option ppf pp optional =
  match optional with
  | Some data ->
    pp ppf data
  | None ->
    Format.fprintf ppf "None"

let pp_string ppf s =
  Format.fprintf ppf "%s" s

let pp_hex ppf x =
  Format.fprintf ppf "0x%x" x

      (* 
  match list with
  | [] -> Format.fprintf ppf "@[@]"
  | x::[] -> Format.fprintf ppf "@[%a@]" pp x
  | x::xs ->
    Format.fprintf ppf "@[<v>%a@ " pp x;
    List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
    Format.fprintf ppf "@]"
 *)
