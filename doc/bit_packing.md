# Concepts

Frame of reference (FOR) comprehension is a scheme for compressing small batches of integers using bit packing. It takes advantage of the fact that numbers are likely to be close together and subtracting the numbers by the smallest one can reduce the bits required to pack them. 

It is used to speed up copying data from the main memory into the cpu cache. The goal of the algorithm is to be faster than copying uncompressed data. Tt has to be very fast and cheap and to be easy to vectorize.

*note:* Lucene appears to the bit packing part but does not subtract the minimum.

Patched frame of reference (PFOR) extends FOR to handle a few exceptionally larger numbers in the batch. It treats a subset of the numbers as exceptions and stores them separately. In lucene only the most significant suffix bits of up to 8 bits are stored separately.

## Bit packing

Consider packing a array of numbers each up to 8 bits in length. Suppose the length of the input array is a multiple of 8. The number of bytes required for the packed representation will be the length of the array multiplied by the number of bits per number divided by the size of a byte.

Let the length of the output packed byte array be n.Take the first n numbers for the input array and write them into each byte in the output array. For smaller bit lengths this process can be repeated until there is no space to fit the numbers without overflowing. If there are numbers still available for packing, pack them into the remaining bits.

The pattern can be extended to 16 and 32 bits. 

## Parallel bit packing

This can be parallelized by using 64bit longs to process multiple arrays at the same time. For example 8 bytes can be processed in parallel. Suppose the array to be packed contains 128 numbers, each containing 8 or fewer bits. The original array can be split into 8 sub arrays containing 16 numbers. These arrays can be transformed into an array of 16 longs. The original algorithm can then be applied on each byte in the long array in parallel.

The logic can be extended to the 16 bits with four parallel streams and 32 bits with 2 parallel streams. 

# Patching

The numbers are processed in batches of 128

Prefixes for some of the the larger numbers are stored separately. Numbers that use more bits that the 8th largest number in the batch will have their prefixes stored. Once the prefixes are extracted along with their indexes, the numbers in the batch will all have at most as many bits as the 8th largest number in the original batch.

The prefix count and the number of bits required to pack each number are stored in a single byte. The number of bits required to encode the prefix count is 3 and 5 bits are available for the bit count. Note that the bit count will be number of bits required for the original 8th largest number. The three most significant bits will be the prefix count and the remaining will be the bit count. 

The byte needs to be written before packing the data, a special case needs to be tanken into account before writing the byte which is described in the next paragraph.

There is a special case when the numbers are at most 8 bits long and they are all equal after removing the prefixes. The bit count in the byte described earlier will be set to zero. The equal number is stored as a vint. Each prefix is written as a byte along with its index, first the index is written as a byte followed by the prefix shifted left by the number of bits in the previously stored vint. This reconstructing the original numbers to be quite simply do an or of the common suffix int and the prefixes for indexes where the prefix is stored.

When not applying the special case. The ints after removing the prefixes are stored using the bit packing algorithm. the results of the algorithm are written as little endian longs.

Finally, the exceptions are written 1 byte for the index and one byte for the prefix.

## Resources
* [A discussion on the ocaml list about vectorization for the stringaf library](https://discuss.ocaml.org/t/some-simd-in-your-ocaml/6367)
* [How auto vectorization works in java](http://daniel-strecker.com/blog/2020-01-14_auto_vectorization_in_java/)
* [A description of the bit packing scheme used in tantivy, Lucene's is based on this](https://fulmicoton.com/posts/bitpacking/)
* https://lemire.me/blog/2012/02/08/effective-compression-using-frame-of-reference-and-delta-coding/
