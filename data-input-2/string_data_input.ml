
module Bytes_source = Bytes_source.Make(String_bytes)

include Bytes_source
include Data_input.Make(Bytes_source)