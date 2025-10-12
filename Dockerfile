# syntax=docker/dockerfile:1.7

FROM ubuntu:24.04

# set a directory for the app
WORKDIR /setup

# copy all the files to the container
COPY . .

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'

# install dependencies
RUN apt-get update && apt-get install -y curl locales libz3-dev && \
    locale-gen en_US.UTF-8

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack build --no-terminal > /tmp/stack_deps.log

# define the port number the container should expose
EXPOSE 3000

# run the command
CMD ["stack", "run"]