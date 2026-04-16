# syntax=docker/dockerfile:1.20

FROM ubuntu:24.04
HEALTHCHECK --interval=300s --timeout=60s --retries=3 CMD curl -f http://localhost:3000 || exit 1

RUN useradd -m user
# set a directory for the app
WORKDIR /home/user

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'

# install prerequisite packages and Stack
RUN apt-get update && apt-get install -y \
    curl=8.5.0-2ubuntu10.8 locales=2.39-0ubuntu8.7 libz3-dev=4.8.12-3.1build1 libpcre3-dev=2:8.39-15build1 &&\
    locale-gen en_US.UTF-8 && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*
USER user

# build dependencies
COPY --chown=user stack.yaml package.yaml codeworld-demo.cabal ./
RUN stack build --no-terminal --dependencies-only

# build app
COPY --chown=user --parents app src static templates ./
RUN stack build --no-terminal

# define the port number the container should expose
EXPOSE 3000

# run the command
CMD ["stack", "run"]
