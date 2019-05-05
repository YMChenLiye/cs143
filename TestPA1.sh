#! /bin/bash

bash Docker/BuildDocker.sh

docker run --rm -it -v $PWD/write-up:/root/workspace/write-up compilers sh -c \
"mkdir build && cd build && make -f /usr/class/cs143/assignments/PA2/Makefile \
&& cp ../write-up/PA1/cool.flex . \
&& perl ../grading/pa1-grading.pl"
