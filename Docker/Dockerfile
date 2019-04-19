FROM 32bit/ubuntu:16.04

COPY student-dist.tar.gz /usr/class/

RUN apt-get update && apt-get install -y flex bison build-essential csh libxaw7-dev wget vim

RUN cd /usr/class \
    && tar -xf student-dist.tar.gz \
    && rm student-dist.tar.gz

ENV PATH=/usr/class/cs143/bin:$PATH

COPY grading /root/workspace/grading
#COPY write-up /root/workspace/write-up

WORKDIR /root/workspace
