#! /bin/bash

bash Docker/BuildDocker.sh

docker run --rm -it -v $PWD/write-up:/root/workspace/write-up compilers bash -c \
"mkdir build && cd build && make -f /usr/class/cs143/assignments/PA5/Makefile \
&& perl ../grading/pa4-grading.pl"
