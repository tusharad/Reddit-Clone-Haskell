FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y \
    curl \
    build-essential \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo-dev \
    xz-utils \
    zlib1g-dev \
    git \
    libpq-dev \
    software-properties-common && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- --yes

ENV PATH="/root/.ghcup/bin:${PATH}"

RUN ghcup install stack 2.15.7

RUN stack --version

WORKDIR /app

COPY . /app

CMD ["sleep", "infinity"]
