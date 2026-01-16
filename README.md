# codeworld-demo

A single page application showcasing task tests written with [codeworld-tasks](https://github.com/fmidue/codeworld-tasks).


## Provisionary Hosting

This demo page is being hosted at [Render](https://codeworld-tasks-demo.onrender.com/) for testing purposes.
It has extremely limited resources and could possibly crash if receiving too many requests at once.
It may also take up to a minute to load the page initially, due to forced shutdown on inactivity for free users.

## Run the Application Locally

### With Docker

1. Install [Docker Engine](https://docs.docker.com/engine/install/) or [Docker Desktop](https://docs.docker.com/get-started/get-docker/)
1. `docker build -t codeworld-demo .` in the root directory of this repository
1. `docker run -p 3000:3000 codeworld-demo`

### With Stack (Unix only)

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/#__tabbed_2_1)
1. Install the z3 theorem prover (`sudo apt-get install libz3-dev` or similar)
1. run `stack run` in the root directory of this repository.

All necessary packages are installed in a sandboxed environment.
The page will then be accessible at `localhost:3000`.
