
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
        x:Int <- 5;
        foo(y:Int):SELF_TYPE { { x <- y; self; } };
};
class B inherits A {
};


class Main inherits IO {
  main():Object { 
    let x:B <- new B in {
                         if x = x then 0 else abort() fi;
                }
};
};