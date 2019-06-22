#! /bin/bash

bash Docker/BuildDocker.sh

docker run --rm -it -v $PWD/write-up:/root/workspace/write-up compilers bash -c \
"mkdir build && cd build && make -f /usr/class/cs143/assignments/PA4/Makefile \
&& cp ../write-up/PA3/* . \
&& sed -i '/CFLAGS=/ s/$/ -std=c++11/' Makefile \
&& sed -i '/CSRC=/ s/$/ TypeCheck.cc/' Makefile \
&& perl ../grading/pa3-grading.pl"
