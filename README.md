# Shiny app to Set Up PostgreSQL table in DB from XLSX files

## Requirements
* Installation of [PostgreSQL](https://www.postgresql.org/download/)
* On Mac, use brew or [this app](https://postgresapp.com/) is very good and easy to set up
* A user and database to which you can connect ([I recommend using psql])(https://www.enterprisedb.com/postgres-tutorials/how-create-postgresql-database-and-users-using-psql-and-pgadmin)

## Usage
* Log in using credentials (required)
* Load XLSX or RDS file(s) if creating a new table, or load an extant table
* Save table as RDS
* Simply close the browser tab to quit

## NBs
* Package was written for a specific use case with specific XLSX inputs
* It should however be extensible to any inputs by changing the parsing
