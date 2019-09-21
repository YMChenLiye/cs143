
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Bazz inherits IO {

     h : Int <- 1;

     g : Foo  <- case self of
                        n : Bazz => (new Foo);
                        n : Foo  => n;
                  esac;

     i : Object <- printh();

     printh() : Int { { out_int(h); 0; } };

     doh() : Int { (let i: Int <- h in { h <- h + 1; i; } ) };
};

class Foo inherits Bazz {

     b : Int <- g.doh() + doh() + printh();

     doh() : Int { (let i : Int <- h in { h <- h + 2; i; } ) };

};

class Main inherits IO {
  a : Bazz <- new Bazz;
  b : Foo <- new Foo;

  main():Object { 
    "do nothing"
};
};