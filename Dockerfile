FROM ubuntu:jammy
COPY  ./target/mpfs-exe ./mpfs-exe
COPY templates templates
COPY static static
CMD ["./mpfs-exe"]
