# haskread-platform-be

Platform backend of haskread.

## Instructions to setup local development environment

### Prerequisites
- [Docker](https://www.docker.com/)
- [Docker-compose](https://docs.docker.com/compose/)

### Clone the repository

```bash
$ git clone git@github.com:tusharad/Reddit-Clone-Haskell.git
```

### Switch to platform-be directory

```bash
$ cd haskread-platform-be
```

### Up the docker compose

```bash
$ docker compose up
```

This command will:
 - Start a postgresql database with all schema created.
 - Start the container which includes haskell compiler tool (stack)
 - The project source code will be mounted to container and postgresql database 
   will be mounted at .dockermnt/pg_db/

Note:
    - Docker-compose command be either `docker-compose up` or `docker compose up` 
      depending upon your installation.

### Build the project using make command 

```bash
$ make build-be-platform
```

### Run the project

```bash
$ make run-be-platform
```

Note:
 - Make sure the environment variables in `env.dhall` are correct for 
   `docker-compose.yml's` postgresql env.
 - Feel free to change them as per your needs.

### Run tests

```
$ make startTest
```

Note:
 - For running tests, the application uses `testEnv.dhall`.
