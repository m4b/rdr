open ByteCoverage

let debug = false

let compute_header_coverage header m =
  let size = (header.MachHeader.ncmds * header.MachHeader.sizeofcmds) + MachHeader.sizeof_mach_header in
  let range_start = 0 in
  let range_end = size + range_start in
  let extra = "mach header and load commands meta data" in
  ByteCoverage.add range_start
              {size; understood = true; kind = Meta;
               range_start; range_end; extra} m
