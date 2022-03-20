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
 * temporary_transitions - The first word in the input list represented as a list of temporary state transitions the
   first word's out will be found in the first element of the list. This is the representation of the current word
   being processed.
 * next_word - A list of chars representing the next word
 * next_output - An output representing the output for the next word
 
## Example

If the first word is "cat" -> "bar"

temporary_transitions will be

```ocaml
 [
   {output = "bar"; char = 'c'; from_state = { transitions = []; final_output = None } }
   {output = ""; char = 'a'; from_state = { transitions = []; final_output = None } }
   {output = ""; char = 't'; from_state = { transitions = []; final_output = Some "" } }
 ]
```

If the next word is "cab" -> "bat", temporary_transitions will be updated to

```ocaml
 [
   {output = "ba"; char = 'c'; from_state = { transitions = []; final_output = None } }
   {output = ""; char = 'a'; from_state = { transitions = []; final_output = None } }
   {output = "t"; char = 'b'; from_state = { transitions = [{char = 't'; output = "r", target = compiled_node}]; final_output = Some "" } }
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

This can be represented as a left fold using the tuple (output * output) and a list to accumulate updated transitions.

The first output is what ever is left over from the output of the new word being added, from previous applications
of the function. The second output is the output accumulated from previous transitions as a remainder.

The input to the fold will be (new_output, empty_output) and an empty list. As this is a fold left, the list will
accumulate the updated temporary state transitions in a reverse order.


```ocaml
val push_output: (new_output: output * old_output: output)
  -> transition: temporary_state_transition
  -> (remaining_new_output: output * remaining_old_output * updated_transition: temporary_state_transition)
```

#### Invariants

 * new_output + transition.output = updated_transition.output + remaining_new_output
 * old_output + transition.output = updated_transition.output + remaining_old_output
 * for all transitions in the from state as ft_transition and ft_updated_transition
   * old_output  + ft_transition = ft_updated_transition
 * old_output + final_output = updated_final_output

### Compile the suffix

This can be represented as a right fold.

The initial value will be a compiled empty final state.

```ocaml

let compile_temporary_state_transition temporary_state_transition compiled_next_state =
  let { ch; output; from_state } = temporary_state_transition in
  let from_state = State.set_transition from_state ch output compiled_next_state in
  compile_state from_state
```

### Update a temporary state transition with a compiled suffix

```ocaml
val update_common_state_transition:  
  new_output: output
  -> old_output: output
  -> common_state_transition: temporary_state_transition
  -> compiled_suffix_state: compiled_state
  -> temporary_state_transition

```

Construct a new temporary state transition. The new transition's output will be the new_output and will have an
additional state transition pointing to compiled state.

### Fill the remaining letters of the new word's suffix

Construct a list of temporary_state_transitions from the remaining letters in the new word's suffix.

### Concatenate the three components

This would be
```ocaml
updated_prefix ++ [updated_common_state_transition] ++ remaining_suffix
```
## Case 2

The length of the prefix is the same as the length of the current word. This can happen when the next word
contains the current word as a prefix. For example, cat and catamaran.

### Split the current word

The current word can be split into a prefix and the last state transition.

### Push the outputs up the prefix

This will be the same as for case 1

#Update the last state transition

* Add the remaining output for the old word and the output of the last state transition as the final output
* Update the output to be the remaining output of the new word.

### Extend the current word

When creating the remaining suffix set the final_output of the first from_state.

## Concatenate the updated prefix and the remaining suffix

```ocaml
updated_prefix ++ [updated_last_state_transition] ++ remaining_suffix
```

