# haskread-platform-be

Platform backend of haskread.

## Instructions to setup local development environment

### Prerequisites

- [Docker](https://www.docker.com/) For PostgreSQL setup.
- Install GHC, Cabal and Stack from [GHCup](https://www.haskell.org/ghcup/).
- Note: This setup assumes you are using linux environment though Windows and mac will have similar commands.

### Clone the repository

```bash
$ git clone git@github.com:tusharad/Reddit-Clone-Haskell.git
```

### Switch to platform-be directory

```bash
$ cd haskread-platform-be
```

### Start the PostgreSQL Database using make command

```bash
$ make startDB
```

This command will:
 - Start a postgresql database.
 - The postgresql database will be mounted at .dockermnt/pg_db/

For the first time, run the (schema.sql)[./Scripts/DB/schema.sql] script to initiate database.

```bash
$ docker cp Scripts/DB/schema.sql haskread_local_pg_db:/tmp/schema.sql
$ docker exec -i haskread_local_pg_db psql -d haskread_local_db -U tushar -f /tmp/schema.sql
```

### Run the project using make command 

Make sure you have stack installed.

```bash
$ make be-haskread-platform-run
```

Note:
 - Make sure the environment variables in `env.dhall` are correct feel free to change them as per your needs.

With this, the backend application will start running at port 8085.

### Run tests

```
$ make startTestDB
$ make startTest
```

Note:
 - For running tests, the application uses `testEnv.dhall`.

## TODO

- Removing jwt tokens from browser's local storage once they are expired.
