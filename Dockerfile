# syntax=docker/dockerfile:1.7

FROM ubuntu:24.04
HEALTHCHECK --interval=300s --timeout=60s --retries=3 CMD curl -f http://localhost:3000 || exit 1

RUN useradd -m user
# set a directory for the app
WORKDIR /home/user

# copy all the files to the container
COPY --chown=user . .

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'

# install dependencies
RUN apt-get update && apt-get install -y \
    curl=8.5.0-2ubuntu10.6 locales=2.39-0ubuntu8.6 libz3-dev=4.8.12-3.1build1 && \
    locale-gen en_US.UTF-8 && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*
USER user

RUN stack build --no-terminal > /tmp/stack_deps.log

# define the port number the container should expose
EXPOSE 3000

# run the command
CMD ["stack", "run"]
