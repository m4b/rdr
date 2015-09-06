open Config

let analyze config binary =
  let pe = PE.get binary in
  PE.show pe
