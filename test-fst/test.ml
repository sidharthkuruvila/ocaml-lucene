let () =
  Alcotest.run "Fst unit tests"
  [
    "Fst", Test_fst.tests;
    "Acyclic transducer", Test_acyclic_transducer.tests;
    "String output", Test_string_output.tests;
    "Byte array fst reader", Test_byte_array_fst_reader.tests;
    "Byte array fst writer", Test_byte_array_fst_writer.tests;
    "Working with lists", Test_lists.tests;
    "Monad", Test_monad.tests;
    "Byte array fst compiler", Test_byte_array_fst_compiler.tests;
  ]
