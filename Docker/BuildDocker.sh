#! /bin/bash

cd Docker
docker build . -t compilers --build-arg http_proxy=http://web-proxy.tencent.com:8080
cd ..
