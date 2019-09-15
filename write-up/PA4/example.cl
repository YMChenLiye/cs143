
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Base inherits IO
{
  identify() : Object
  {
    out_string( "base\n" )
  };

  duplicate() : Base
  {
    copy()
  };
};


class Derived inherits Base
{
  identify() : Object
  {
    out_string( "derived\n" )
  };
};

class Main inherits IO {
  i:Int;
  main():Object { 
    (new Derived).duplicate().identify()
};
};