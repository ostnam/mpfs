FROM ubuntu:jammy
COPY  ./target/mpfs-exe ./mpfs-exe
COPY templates templates
COPY static static
RUN apt update && apt install -y ca-certificates
CMD ["./mpfs-exe"]
