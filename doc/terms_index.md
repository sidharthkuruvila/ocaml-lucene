The terms index is split into two files an index and a dictionary


The index is an FST(Finite State Transducer), that takes, a term and returns the path to the block where that term's data is stored in the dictionary.


### The implementation of the FST.

The FST is represented in terms of the arcs between states. 

It starts with a virtual arc that points to the first state. The first state is a final state as the empty string is considered to be a match. The transducer in this case returns an empty output.

The FST's input is represented by a byte array backed data input with random access. 

The value of a state's is the position of the state related data and following arcs in the FST

Labels are individual bytes in the target string.

To transition based on a label. The first step is to change the position on the FST's input to the position stored in the state.

A byte is read to get the representation of the arcs out of the state.

There are thee representations

1. Direct Addressing
2. Binary Search
3. Linear Scan

### Direct Addressing

With direct addressing it is possible to jump straight to the arc that matches the next label.

A bitset is provided to speed up the lookup, if a labels is not present in the bitset it won't be a valid arc.

The structure of a direct addressing node is as follows

 * num_arcs: vint
 * bytes_per_arc: vint
 * presence_bit_table: byte[(num_arcs + 7) lsl 3]
 * first_label: byte
 * arc_list: bytes_per_arc * `number of arcs with set bits`

The presence bit table acts as a space optimization measure. If the bit indexed by the label is set. The count of set bits upto that bit gives the index of the arc. The bit index can be calculated as label - first_label.

Each arc has the following structure


