The terms index is split into two files an index and a dictionary


The index is an FST(Finite State Transducer). It works similar to a prefix tree where it points to the location of the prefix of a term in the dictionary.


# The implementation of the FST.

The FST is represented in terms of the arcs between states. 

It starts with a virtual arc that points to the first state. The first state is a final state as the empty string is considered to be a match. The transducer in this case returns an empty output.



The value label of a state is the position of the state related data and following arcs in the FST.

Transition labels are the individual bytes in the term string.

To transition based on a label. The first step is to change the position on the FST's input to the position stored in the state.

A byte is read to get the representation of the arcs out of the state.

There are thee representations

1. Direct Addressing
2. Binary Search
3. Linear Scan

## Direct Addressing

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
A flag byte containing the following bits

* bit_arc_has_output: Arc has output
* Arc has final output
* Next node is stop node
* This arc is the final arc
* bit_target_next: The next target state immediately follows the current node
* This is the last arc in the list

If bit_arc_has_output is set read the output bytes as a string
If bit_arc_has_final_output is set read final output bytes as a string

If the bit_target_next flag is set simply jump to the next node by jumping to the end of the current one.
Otherwise read the position from arc data as a vint.


## Constructing the paths

The path construction seems a bit strange.

If the arc is a final arc, but the next node is not the stop node the path is the final output of the current arc.
If the next node is the stop node then the path is the output of the current arc.

### Structure of the path

The path is a byte array that can be wrapped by a data input.

Read a vlong code. The lowest two bytes are bit flags. The code can be right shifted to get a pointer in the dictionary file to the starting block to search for the term. 

The bit flags are

* bit_has_terms: The block has term data
* bit_is_floor: Is a floor block

Question: What does it mean to have terms and what is a floor block.

The next step is to read the next floor label byte. This gives the upperbound of labels found in the current block
