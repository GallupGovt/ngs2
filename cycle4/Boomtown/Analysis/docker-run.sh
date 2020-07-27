#/usr/bin/bash

mkdir ~/.ngs2
docker build -t ngs2:latest .
docker run -v ~/.ngs2:/app/output ngs2:latest
