open Config

let analyze config binary =
  let pe = PE.get binary in
  Format.printf "%a\n" PE.pp pe
