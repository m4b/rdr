open ByteCoverage

let debug = false

let compute_header_coverage header data =
  let size = (header.MachHeader.ncmds * header.MachHeader.sizeofcmds) + MachHeader.sizeof_mach_header in
  let range_start = 0 in
  let range_end = size + range_start in
  let extra = "mach header and load commands meta data" in
  ByteCoverage.add
              {size; understood = true; tag = Meta;
               range_start; range_end; extra} data


let compute header load_commands size =
  compute_header_coverage header ByteCoverage.empty
  |> ByteCoverage.create size

