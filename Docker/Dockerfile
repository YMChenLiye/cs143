FROM i386/ubuntu:16.04

RUN sed -i s@/archive.ubuntu.com/@/mirrors.aliyun.com/@g /etc/apt/sources.list
RUN apt-get update && apt-get install -y flex bison build-essential libxaw7-dev wget vim tcsh

COPY student-dist.tar.gz /usr/class/

RUN cd /usr/class \
    && tar -xf student-dist.tar.gz \
    && rm student-dist.tar.gz

ENV PATH=/usr/class/cs143/bin:$PATH

COPY grading /root/workspace/grading

WORKDIR /root/workspace
