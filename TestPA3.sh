#! /bin/bash

bash Docker/BuildDocker.sh

docker run --rm -it -v $PWD/write-up:/root/workspace/write-up compilers sh -c \
"mkdir build && cd build && make -f /usr/class/cs143/assignments/PA4/Makefile \
&& cp ../write-up/PA3/* . \
&& perl ../grading/pa3-grading.pl"
