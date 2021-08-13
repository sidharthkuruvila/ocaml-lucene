type t = {
  dir: string;
  segment_info: Segment_info.t;
  field_infos: Field_infos.t;
}

let make ~dir ~segment_info ~field_infos =
  { dir; segment_info; field_infos }

