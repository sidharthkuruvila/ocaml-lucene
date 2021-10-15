# Concepts


Frame of Reference (FOR) compression
Patched Frame of Reference (PFOR)

# Bit packing strategies



## Linear packing
A simple linear algorithm to pack positive numbers up to 32 bits in length


Pack into byte moves bits from number_to_pack to byte_to_fill. After it runs one of three things will happen, the byte will be filled, the number_to_pack will be empty or both.
```ocaml
let pack_into_byte byte_to_fill bits_to_fill number_to_pack bits_to_pack =
  let packable_bits = min bits_to_fill bits_to_pack in
  let byte = (byte_to_fill lsl packable_bits) lor 
    ((number_to_pack lsr (bits_to_pack - packable_bits)) land 0xFF) in
  let remaining_number = number_to_pack land ((1 lsl packable_bits) - 1)  in
      (byte, bits_to_fill - packable_bits, remaining_number, bits_to_pack - packable_bits)
```

```ocaml
let pack_bits l num_size =
  let rec pack_next_int l byte_to_fill bits_to_fill =
    match l with
    | n::rest -> pack rest byte_to_fill bits_to_fill n num_size
    | [] when bits_to_fill != 8 -> [byte_to_fill] 
    | [] -> []
  and pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack = 
    if bits_to_pack = 0 then
           pack_next_int l byte_to_fill bits_to_fill
    else if bits_to_fill = 0 then
      byte_to_fill::(pack l 0 8 num_to_pack bits_to_pack)   
    else 
      let (byte_to_fill, bits_to_fill, num_to_pack, bits_to_pack)
        = pack_into_byte byte_to_fill bits_to_fill num_to_pack bits_to_pack in
      pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack in
  pack_next_int l 0 8 
```


The bits will be packed in msb order.

## Parallel packing



https://lemire.me/blog/2012/02/08/effective-compression-using-frame-of-reference-and-delta-coding/

https://fulmicoton.com/posts/bitpacking/


# Packing positive 32 bit ints

The logic involves packing batches of 128 numbers at a time. The numbers are restricted to 32 bits in size.

Prefixes for the larger numbers that can be stored separately, these prefixes can be at most 1 byte lon g. In the implementation the number of prefixes is restricted to at most 7 numbers. Or more explicitly numbers larger than the 8th largest number.

There is a special case when the numbers are at most 8 bits long and they are all equal after removing the prefixes. The number of prefixes to store is shifted to the left 5 bits and written as a byte. The equal number is stored as a vint. Each prefix is written as a byte along with its index, first the index is written as a byte followed by the prefix shifted left by the number of bits in the previously stored vint. This reconstructing the original numbers to be quite simply do an or of the common suffix int and the prefixes for indexes where the prefix is stored.

When not applying the special case. The number of prefixes and the number of bits required to store the ints after the prefixes have been removed are stored in a single byte. The number of prefixes is shifted left 5 bits and ored with the number of bits required after the prefixes have ben removed, the byte is then written. The ints without prefixes are stored using a specific format.

# Packing a list of positive ints with byte boundaries

This logic works to pack batches of  128 numbers each up to 32 bits long.

There three cases

## The number of bits per int is less than or equal to 8


