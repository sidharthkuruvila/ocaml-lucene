
module Bytes_source = Bytes_source.Make(Mmapped_file_bytes)

include Bytes_source
include Data_input.Make(Bytes_source)