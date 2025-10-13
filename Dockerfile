# syntax=docker/dockerfile:1.7

FROM ubuntu:24.04

# set a directory for the app
WORKDIR /setup

# copy all the files to the container
COPY . .

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'

# install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends curl locales libz3-dev && \
    rm -rf /var/lib/apt/lists/* && \
    locale-gen en_US.UTF-8

RUN set -o pipefail && curl -sSL https://get.haskellstack.org/ | sh
RUN stack build --no-terminal > /tmp/stack_deps.log

# define the port number the container should expose
EXPOSE 3000

# run the command
CMD ["stack", "run"]
