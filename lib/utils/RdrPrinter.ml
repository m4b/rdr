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

      (* 
  match list with
  | [] -> Format.fprintf ppf "@[@]"
  | x::[] -> Format.fprintf ppf "@[%a@]" pp x
  | x::xs ->
    Format.fprintf ppf "@[<v>%a@ " pp x;
    List.iter (fun x -> Format.fprintf ppf "%a@ " pp x) xs;
    Format.fprintf ppf "@]"
 *)
