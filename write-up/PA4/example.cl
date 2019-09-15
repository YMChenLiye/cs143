
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

  mul(x:Int, y:Int):Int{
    x*y
  };
  div(x:Int, y:Int):Int{
    x/y
  };
  main():Object { 
    {

    out_int(mul(1,2));
    }
};
};