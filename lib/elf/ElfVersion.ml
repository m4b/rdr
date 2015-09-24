open Binary

type elf32_verdef = {
    vd_version: int [@size 4];
    vd_flags: int [@size 4];
    vd_ndx: int [@size 4];
    vd_cnt: int [@size 4];
    vd_hash: int [@size 4];
    vd_aux: int [@size 4];
    vd_next: int [@size 4];
  }

type elf64_verdef = {
    vd_version: int [@size 8];
    vd_flags: int [@size 8];
    vd_ndx: int [@size 8];
    vd_cnt: int [@size 8];
    vd_hash: int [@size 8];
    vd_aux: int [@size 8];
    vd_next: int [@size 8];
  }

type elf32_verdaux = {
    vda_name: int [@size 4];
    vda_next: int [@size 4];
  }

type elf64_verdaux = {
    vda_name: int [@size 8];
    vda_next: int [@size 8];
  }

type elf32_verneed = {
    vn_version: int [@size 4];
    vn_cnt: int [@size 4];
    vn_file: int [@size 4];
    vn_aux: int [@size 4];
    vn_next: int [@size 4];
  }

type elf64_verneed = {
    vn_version: int [@size 8];
    vn_cnt: int [@size 8];
    vn_file: int [@size 8];
    vn_aux: int [@size 8];
    vn_next: int [@size 8];
  }

type elf32_vernaux = {
    vna_hash: int [@size 4];
    vna_flags: int [@size 4];
    vna_other: int [@size 4];
    vna_name: int [@size 4];
    vna_next: int [@size 4];
  }

type elf64_vernaux = {
    vna_hash: int [@size 8];
    vna_flags: int [@size 8];
    vna_other: int [@size 8];
    vna_name: int [@size 8];
    vna_next: int [@size 8];
  }

let kVER_DEF_NONE = 0

let kVER_DEF_CURRENT = 1

let kVER_DEF_NUM = 2

let kVER_FLG_BASE = 0x1

let kVER_FLG_WEAK = 0x2

let kVER_NDX_LOCAL = 0

let kVER_NDX_GLOBAL = 1

let kVER_NDX_LORESERVE = 0xff00

let kVER_NDX_ELIMINATE = 0xff01

let kVER_NEED_NONE = 0

let kVER_NEED_CURRENT = 1

let kVER_NEED_NUM = 2

let kVER_FLG_WEAK = 0x2

let get_elf32_verdef binary offset :elf32_verdef =
  let vd_version,o = Binary.u32o binary offset in
  let vd_flags,o = Binary.u32o binary o in
  let vd_ndx,o = Binary.u32o binary o in
  let vd_cnt,o = Binary.u32o binary o in
  let vd_hash,o = Binary.u32o binary o in
  let vd_aux,o = Binary.u32o binary o in
  let vd_next = Binary.u32 binary o in
  {vd_version;vd_flags;vd_ndx;vd_cnt;vd_hash;vd_aux;vd_next;}

let get_elf64_verdef binary offset :elf64_verdef =
  let vd_version,o = Binary.u64o binary offset in
  let vd_flags,o = Binary.u64o binary o in
  let vd_ndx,o = Binary.u64o binary o in
  let vd_cnt,o = Binary.u64o binary o in
  let vd_hash,o = Binary.u64o binary o in
  let vd_aux,o = Binary.u64o binary o in
  let vd_next = Binary.u64 binary o in
  {vd_version;vd_flags;vd_ndx;vd_cnt;vd_hash;vd_aux;vd_next;}

let get_elf32_verdaux binary offset :elf32_verdaux =
  let vda_name,o = Binary.u32o binary offset in
  let vda_next = Binary.u32 binary o in
  {vda_name;vda_next;}

let get_elf64_verdaux binary offset :elf64_verdaux =
  let vda_name,o = Binary.u64o binary offset in
  let vda_next = Binary.u64 binary o in
  {vda_name;vda_next;}

let get_elf32_verneed binary offset :elf32_verneed =
  let vn_version,o = Binary.u32o binary offset in
  let vn_cnt,o = Binary.u32o binary o in
  let vn_file,o = Binary.u32o binary o in
  let vn_aux,o = Binary.u32o binary o in
  let vn_next = Binary.u32 binary o in
  {vn_version;vn_cnt;vn_file;vn_aux;vn_next;}

let get_elf64_verneed binary offset :elf64_verneed =
  let vn_version,o = Binary.u64o binary offset in
  let vn_cnt,o = Binary.u64o binary o in
  let vn_file,o = Binary.u64o binary o in
  let vn_aux,o = Binary.u64o binary o in
  let vn_next = Binary.u64 binary o in
  {vn_version;vn_cnt;vn_file;vn_aux;vn_next;}

let get_elf32_vernaux binary offset :elf32_vernaux =
  let vna_hash,o = Binary.u32o binary offset in
  let vna_flags,o = Binary.u32o binary o in
  let vna_other,o = Binary.u32o binary o in
  let vna_name,o = Binary.u32o binary o in
  let vna_next = Binary.u32 binary o in
  {vna_hash;vna_flags;vna_other;vna_name;vna_next;}

let get_elf64_vernaux binary offset :elf64_vernaux =
  let vna_hash,o = Binary.u64o binary offset in
  let vna_flags,o = Binary.u64o binary o in
  let vna_other,o = Binary.u64o binary o in
  let vna_name,o = Binary.u64o binary o in
  let vna_next = Binary.u64 binary o in
  {vna_hash;vna_flags;vna_other;vna_name;vna_next;}
