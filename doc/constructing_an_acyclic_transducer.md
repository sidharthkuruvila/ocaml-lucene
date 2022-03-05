The acyclic transducer construction described in https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.24.3698 forms
the basis of the FST used in Lucene to search for terms in the index.

The fst described by the paper adds a lot of stateful update operations that make alternate implementations of the fst 
tricky. This document describes a simpler version of the algorithm.

# Algorithm

The fst exposes a method called compile_state. The logic of deduplicating states is managed by this function. Once
a state is compiled it cannot be modified.

## Types

![Types](images/acyclic-transducer-types.png "Types used to build the acyclic transducer")

## Initial state
 * current_word - The first word in the input list represented as a list of temporary state transitions
 * next_word - A list of chars representing the next word
 * next_output - An output representing the output for the next word
 
## Example

If the first word is "cat" -> "bar"

current word will be

```ocaml
 [
   {output = "bar"; char = 'c'; from_state = { is_final = false; transitions = []; final_output = ""} }
   {output = ""; char = 'a'; from_state = { is_final = false; transitions = []; final_output = ""} }
   {output = ""; char = 't'; from_state = { is_final = false; transitions = []; final_output = ""} }
 ]
```

If the next word is "cab" -> "bat"

```ocaml
 [
   {output = "ba"; char = 'c'; from_state = { is_final = false; transitions = []; final_output = ""} }
   {output = ""; char = 'a'; from_state = { is_final = false; transitions = []; final_output = ""} }
   {output = "t"; char = 'b'; from_state = { is_final = false; transitions = [{char = 't'; output = "r", target = compiled_node}]; final_output = ""} }
]
```
## Dealing with the existing suffix

The current word's suffix needs to be compiled before next word can become the current word.

There are two cases to be handled

## Case 1

The common prefix is shorter than the length of the current word.

### Split the current word 

Split the list into three parts common_prefix, common_state and compilation_candidates.

In the example, the common_prefix would be the list containing char = 'c' and char = 'a'. The common state would be
the state containing char = 't'. The compilation candidates would be an empty list.

### Push the outputs up the common prefix

This can be represented as a left fold

Initial value will be the next output and an empty list. The list will accumulate the updated temporary state 
transitions in a reverse order. 

```ocaml
val push_output:  (output * temporary_state_transition list)
  -> temporary_state_transition
  -> (output * temporary_state_transition list)
```

For each temporary state transition
 * Update the transition's output to the common prefix
 * Push the suffix of the original output to from_state's transition outputs and the final output
 * Return the suffix of the input output and append the updated transition to the input list

### Compile the suffix

This can be represented as a right fold.

The initial value will be a compiled empty final state.

```ocaml

let compile_temporary_state_transition temporary_state_transition compiled_next_state =
  let { ch; output; from_state } = temporary_state_transition in
  let from_state = State.set_transition from_state ch output compiled_next_state in
  compile_state from_state
```

### Construct the compiled suffix char

```ocaml
val update_common_state_transition:  
  remaining_output: output
  -> common_state_transition: temporary_state_transition
  -> compiled_suffix_state: compiled_state

```

* Add a transition to the from state of common_state_transition from the current char and its output to the compiled_suffix_state
* Create a new temporary state transition with the first char of the new word's suffix and the remaining output the remaining

### Fill the remaining letters of the new word's suffix

Construct a list of temporary_state_transitions from the remaining letters in the new word's suffix.

### Concatenate the three components

This would be
```ocaml
updated_prefix ^^ [updated_common_state_transition] ^^ remaining_suffix
```
## Case 2

The length of the prefix is the same as the length of the current word. This can happen when the next word
contains the current word as a prefix. For example, cat and catamaran.


### Extend the current word 
