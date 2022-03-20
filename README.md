# A port of the lucene index in ocaml

This started out as a project to build a tool visualize Lucene's internal data structures. I very quickly realized that this was an enormous undertaking. And it turns out that Lucene already comes with a builtin tool called Luke.

My current goal is to use this as a learning tool to understand the Lucene index by porting it to ocaml. The intention is to be able to run simple queries on indexes constructed by Lucene.

I try to document what I've learned.

 * [Bit packing](doc/bit_packing.md)
 * [The terms index](doc/terms_index.md)
 * [Constructing an acyclic transducer](doc/constructing_an_acyclic_transducer.md)