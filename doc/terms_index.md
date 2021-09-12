The terms index is split into two files an index and a dictionary


The index is an FST(Finite State Transducer). It works similar to a prefix tree where it points to the location of the terms with the same prefix the dictionary.


# The implementation of the FST.

The FST is represented in terms of the arcs or transitions between state nodes. Some states in the fst can be final states, these match to a prefix which points to a block in a dictionary

E.g. We have a simple fst a -> b -> c where b is a final state, the output of the transducer at that point will be a valid path to a block. The output for non final states will always be empty.  

It starts with a virtual arc that points to the first state. The first state is a final state as the empty string can be considered a match. The transducer in this case returns an empty output.

States do not have labels, instead the incoming arc contains a pointer to the next state's data.

Transition labels are the individual bytes in the term string.

To transition based on a label. The first step is to change the position on the FST's input to the position of the state's data.

A byte is read to get the representation of outgoing arcs.


![FST](fst.png "FST")


There are thee representations

1. Direct Addressing
2. Binary Search
3. Linear Scan

## Direct Addressing

With direct addressing it is possible to jump straight to the arc that matches the next label.

A bitset is used to speed up the lookup, if a label is not present in the bitset it won't be a valid arc.

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


## Constructing the pointer data to the dictionary

As described earlier the fst's output is a pointer to the matching suffixes in the dictionary. There are two cases

1. When the incoming arc is a final arc, but the node is not the stop node. The pointer is data stored as the final output of the arc. 
2. When the next node is the stop node. The pointer data is scored as the output of the current arc.

### Structure of the pointer data

![Block path](block_path.png "Block path")

The pointer data is a byte array that can be read as a data input.

Read a vlong code. The lowest two bytes are bit flags. The code can be right shifted to get a pointer in the dictionary file to the starting block to search for the term. 

The bit flags are

* bit_has_terms: The block has term data.
* bit_is_floor: Is a floor block

Question: What is a floor block.

The next step is to read the next floor label byte. This gives the upperbound of labels found in the current block.

If the bit_has_terms is false, the labels lower than the next floor label will not map to any terms.

If the target label is greater than the next floor label read the next block.

# Finding terms in the terms dictionary

Thf block pointer gives the position of the block containing the data in dictionary.

