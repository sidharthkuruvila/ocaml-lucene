let _ = Random.self_init ()

let time_single_run ~repeats ~f =
  let start_time = Unix.gettimeofday () in
  let start_counters = Unix.times () in
  let start_gc_stats = Gc.quick_stat () in
  for _ = 1 to repeats do
    ignore(f ())
  done;
  let end_time =  Unix.gettimeofday () in
  let end_counters = Unix.times () in
  let end_gc_stats = Gc.quick_stat () in
  (start_time, start_counters, start_gc_stats, end_time, end_counters, end_gc_stats)

let display_single_run (start_time, _, _, end_time, _, _) =
  Printf.printf "Time taken = %f\n" (end_time -. start_time)


let random_char () = char_of_int (Random.bits () land 0xff);;

let random_string_bit_vector n =
  String.of_seq (Seq.unfold (fun n -> if n = 0 then None else Some ( random_char(), n - 1)) n)

(*let string_bit_vector_1000 = random_string_bit_vector 1000*)

let string_bit_vector_8 = random_string_bit_vector 8

let _ = print_newline ()


let byte_array_bit_vector_8 =
  Bigarray.Array1.init
    Bigarray.Int8_unsigned
    Bigarray.c_layout 8 (fun i ->  int_of_char (String.get string_bit_vector_8 i))


let repetitions = 1000000

(* Big endian representation of an integer *)
let int64_from_bytes b o =
  let open Int64 in
  let rec loop i n =
    if i = o + 8 then
      n
    else
      let b = String.get b i in
      let m = logor (shift_left n 8) (of_int (int_of_char b)) in
      loop (i + 1) m in
  loop 0 0L


let int_from_bytes b o =
  let b1 = String.get b (o + 0) |> int_of_char in
  let b2 = String.get b (o + 1) |> int_of_char in
  let b3 = String.get b (o + 2) |> int_of_char in
  let b4 = String.get b (o + 3) |> int_of_char in
  (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

let int_from_bytes_2 b o =
  let b1 = int_of_char (String.get b (o + 0))  in
  let b2 = int_of_char (String.get b (o + 1))  in
  let b3 = int_of_char (String.get b (o + 2)) in
  let b4 = int_of_char (String.get b (o + 3))  in
  (b1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

module Count_bytes_linear = struct

  let mask2 =  0x55555555
  let mask4 =  0x33333333
  let mask8 =  0x0F0F0F0F
  let count_bits8 n =
    let count2 = ((n lsr 1) land mask2) + (n land mask2) in
    let count4 = ((count2 lsr 2) land mask4) + (count2 land mask4) in
    let count8 = ((count4 lsr 4) land mask8) + (count4 land mask8) in
    count8

  let count_bits8_linear b =
    let rec loop b c =
      if b = 0 then
        c
      else
        loop (b lsr 1) (c + (b land 1)) in
    loop b 0

  let count_bits_in_byte_array b =
    let rec sum_bytes n c =
      if n = String.length b then
        c
      else
        sum_bytes (n+1) (c + count_bits8 (int_of_char (String.get b n))) in
    sum_bytes 0 0

  let count_bits_in_byte_array_unrolled b =
    let i1 = count_bits8 (int_of_char (String.get b 0)) in
    let i2 = count_bits8 (int_of_char (String.get b 1)) in
    let i3 = count_bits8 (int_of_char (String.get b 2)) in
    let i4 = count_bits8 (int_of_char (String.get b 3)) in
    let i5 = count_bits8 (int_of_char (String.get b 4)) in
    let i6 = count_bits8 (int_of_char (String.get b 5)) in
    let i7 = count_bits8 (int_of_char (String.get b 6)) in
    let i8 = count_bits8 (int_of_char (String.get b 7)) in
    i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8

  let test_fn () =
    count_bits_in_byte_array_unrolled string_bit_vector_8

  let measure () =
    let res = time_single_run ~repeats:repetitions ~f:test_fn in
    display_single_run res

end

module Count_bits64 = struct
  let mask2 =  0x5555555555555555L
  let mask4 =  0x3333333333333333L
  let mask8 =  0x0F0F0F0F0F0F0F0FL
  let mask16 = 0x00FF00FF00FF00FFL
  let mask32 = 0x0000FFFF0000FFFFL
  let mask64 = 0x00000000FFFFFFFFL
  let count_bits64 n =
    let open Int64 in
    let count2 = add (logand (shift_right_logical n 1) mask2) (logand n mask2) in
    let count4 = add (logand (shift_right_logical count2 2) mask4) (logand count2 mask4) in
    let count8 = add (logand (shift_right_logical count4 4) mask8) (logand count4 mask8) in
    let count16 = add (logand (shift_right_logical count8 8) mask16) (logand count8 mask16) in
    let count32 = add (logand (shift_right_logical count16 16) mask32) (logand count16 mask32) in
    let count64 = add (logand (shift_right_logical count32 32) mask64) (logand count32 mask64) in
    Int64.to_int count64

  let count_bits_in_byte_array b =
    count_bits64 (int64_from_bytes b 0)


  let test_fn () =
    count_bits_in_byte_array string_bit_vector_8

  let measure () =
    let res = time_single_run ~repeats:repetitions ~f:test_fn in
    display_single_run res

end

(*
0.013717
0.069750
0.005963

*)
module Count_bits32 = struct
  let mask2 =  0x55555555
  let mask4 =  0x33333333
  let mask8 =  0x0F0F0F0F
  let mask16 = 0x00FF00FF
  let mask32 = 0x0000FFFF
  let count_bits32 n =
    let count2 = ((n lsr 1) land mask2) + (n land mask2) in
    let count4 = ((count2 lsr 2) land mask4) + (count2 land mask4) in
    let count8 = ((count4 lsr 4) land mask8) + (count4 land mask8) in
    let count16 = ((count8 lsr 8) land mask16) + (count8 land mask16) in
    let count32 = ((count16 lsr 16) land mask32) + (count16 land mask32) in
    count32

  let count_bits_in_byte_array b =
    let i1 = count_bits32 (int_from_bytes_2 b 0) in
    let i2 = count_bits32 (int_from_bytes_2 b 4) in
    i1 + i2


  let test_fn () =
    count_bits_in_byte_array string_bit_vector_8

  let measure () =
    let res = time_single_run ~repeats:repetitions ~f:test_fn in
    display_single_run res
end






let _ =
  (*let tests = [
    "lin_bit_count", "Count bits in byte array linearly", Count_bytes_linear.test_fn;
    "int64_bit_count", "Count bits in byte array by mapping it to an Int64.t", Count_bits64.test_fn;
    "int_bit_count", "Count bits in byte array by mapping it to two ints\n", Count_bits32.test_fn
  ] in*)
  print_endline string_bit_vector_8;
  print_endline (Int64.to_string (int64_from_bytes string_bit_vector_8 0));
  print_endline (Int64.to_string (Int64.add
    (Int64.shift_left (Int64.of_int (int_from_bytes string_bit_vector_8 0)) 32)
    (Int64.of_int (int_from_bytes string_bit_vector_8 4))
    ));
  Printf.printf "L: %d, 64: %d, int: %d\n"
    (Count_bytes_linear.count_bits_in_byte_array_unrolled string_bit_vector_8)
    (Count_bits64.count_bits_in_byte_array string_bit_vector_8)
    (Count_bits32.count_bits_in_byte_array string_bit_vector_8);
  Printf.printf "Count loop with no work\n";
  Count_bits64.measure ();
  Printf.printf "Count bits in byte array linearly\n";
  Count_bytes_linear.measure ();
  Printf.printf "Count bits in byte array by mapping it to a 64 bit int\n";
  Count_bits64.measure ();
  Printf.printf "Count bits in byte array by mapping it to two ints\n";
  Count_bits32.measure ();
  Printf.printf "Count bits in byte array linearly\n";
  Count_bytes_linear.measure ();
  Printf.printf "Count bits in byte array by mapping it to a 64 bit int\n";
  Count_bits64.measure ();
  Printf.printf "Count bits in byte array by mapping it to two ints\n";
  Count_bits32.measure ()