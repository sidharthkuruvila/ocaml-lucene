# Concepts

Frame of Reference (FOR) compression
Patched Frame of Reference (PFOR)

## Parallel bit packing

Consider packing a array of numbers each up to 8 bits in length. Suppose the length of the input array is a multiple of 8. The number of bytes required for the packed representation would be the length of the array multiplied by the number of bits divided by 8.

Let the length of the output packed byte array be n.Take the first n numbers for the input array and write them into each byte in the output array. For smaller bit lengths this process can be repeated until there is no space to fit the numbers without overflowing. If there are numbers available, pack them into the remaining bits.

The pattern can be extended to 16 and 32 bits. 

This can be parallelized by using 64bit longs to process multiple arrays at the same time. For example 8 bytes can be processed in parallel. Suppose the array to be packed contains 128 numbers, each containing 8 or fewer bits. The original array can be split into 8 sub arrays containing 16 numbers. These arrays can be transformed into an array of 16 longs. The original algorithm can then be applied on each byte in the long array in parallel.

The logic can be extended to the 16 bits with four parallel streams and 32 bits with 2 parallel streams. 

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


