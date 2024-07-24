# haskread-platform-be

## How to run

### Prerequisites
- [GHCup](https://www.haskell.org/ghcup/)
- [PostgreSQL](https://www.postgresql.org/download/)

### Setup

1. Clone the repository
```bash
git clone 
```

2. Install dependencies
```bash
cd haskread-platform-be
stack build
```

3. Create the database
```bash
psql -U <username> -c "CREATE DATABASE haskread_dev_db"
```

5. Install the database schema
```bash
psql -U <username> -d haskread_dev_db -f Scripts/DB/Schema.sql
```

6. Run the server
```bash
stack run
```

