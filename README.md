# Shiny app to Set Up PostgreSQL tables in a database from XLSX, RDS and PDF files

## Requirements
* Installation of [PostgreSQL](https://www.postgresql.org/download/)
* On Mac, use brew or [this app](https://postgresapp.com/) is very good and easy to set up
* A username with password for a database to which you can connect ([I recommend using psql](https://www.enterprisedb.com/postgres-tutorials/how-create-postgresql-database-and-users-using-psql-and-pgadmin))

## Usage
* Log in using credentials (required)
* Create a blank table, load XLSX, RDS or correctly formatted PDF file(s) into existing or new tables, or load an extant table and add columns
* Save table as RDS or back into Postgres
* Simply close the browser tab to quit

## NBs
* Previously the parsing was specific to certain inputs but has been revised to make the package more extensible to any input. This uses a modal to rename column names of imported data into the preferred formatting, or into user-defined column naming.
