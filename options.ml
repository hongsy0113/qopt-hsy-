let input_file = ref None

let options =
  [
    ("-input", Arg.String (fun s -> input_file := Some s), "input a QASM program");
  ]
