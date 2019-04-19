#! /bin/bash

bash Docker/BuildDocker.sh

docker run --rm -it -v $PWD/write-up:/root/workspace/write-up compilers sh -c \
"mkdir build && cd build && make -f /usr/class/cs143/assignments/PA3/Makefile \
&& sed -i 's/int curr_lineno;/\/\/int curr_lineno;/g' parser-phase.cc \
&& cp ../write-up/cool.y . \
&& perl ../grading/pa2-grading.pl"
