
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main inherits IO {
  i:Int;
  main():Object { let x:Int <- 0 in {
    while x < 100 loop {
      x <- x + 1;
    } pool;
    out_int(x);
  } };
};

