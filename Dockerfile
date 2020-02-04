# DockerFile for the HYPE model.
# The input data root directory should be mounted as /data
# The filedir.txt in the input directory should only contain the single line: /data/
# The output folder should exist within the mounted volume
FROM ubuntu:bionic
MAINTAINER Gijs van den Oord <g.vandenoord@esciencecenter.nl>
RUN apt-get update

# Prerequisite packages
RUN apt-get install -y wget git build-essential g++ make cmake curl automake libtool pkg-config gfortran

# Build grpc from source
RUN git clone --depth=1 -b v1.20.0 https://github.com/grpc/grpc /opt/grpc
WORKDIR /opt/grpc
RUN git submodule update --init --recursive
RUN make install
WORKDIR third_party/protobuf
RUN make install

# Build bmi-c from source
RUN git clone --depth=1 -b v0.2.3 https://github.com/eWaterCycle/grpc4bmi.git /opt/grpc4bmi
WORKDIR /opt/grpc4bmi
RUN git submodule update --init --recursive
RUN mkdir -p /opt/grpc4bmi/cpp/bmi-c/build
WORKDIR /opt/grpc4bmi/cpp/bmi-c/build
RUN cmake ..
RUN make install

# Build grpc4bmi from source
RUN mkdir -p /opt/grpc4bmi/cpp/build
WORKDIR /opt/grpc4bmi/cpp/build
RUN cmake ..
RUN make install

# Build hype-bmi
COPY . /opt/hype-bmi
WORKDIR /opt/hype-bmi/src
RUN make clean
RUN make

VOLUME /data
# WORKDIR /data
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib:/usr/lib/x86_64-linux-gnu/"
ENV PATH="${PATH}:/usr/local/bin"
ENTRYPOINT ["/opt/hype-bmi/src/hype_bmi_server"]
