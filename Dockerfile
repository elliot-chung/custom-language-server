FROM rust

RUN mkdir /home/runtime /home/src /home/target /home/tests

COPY /runtime /home/runtime
COPY /src /home/src
COPY /Cargo.toml /home/Cargo.toml
COPY /server /home/server
COPY /Makefile /home/Makefile

EXPOSE 8080

WORKDIR /home

RUN cargo build

RUN apt-get update && apt-get install nasm

CMD ["./server"]