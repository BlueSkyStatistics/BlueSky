
importMSSQLDBList <- function(server="localhost", database, tablename, user=NULL, password=NULL, port=1433, WinLogin=TRUE, datasetname)
{
	library(DBI)
	library(odbc)

	con = NULL
	# List available drivers
	drivers <- odbc::odbcListDrivers()$name

	# Try to find a SQL Server driver
	sql_driver <- drivers[grepl("SQL Server", drivers, ignore.case = TRUE)][1]

	# Check if found
	if (is.na(sql_driver)) {
	  print("No SQL Server ODBC driver found. Please install 'ODBC Driver 17/18 for SQL Server'.")
	}
	else
	{
		if(WinLogin)#if windows login used
		{				 
			con <- DBI::dbConnect(odbc::odbc(),
			Driver = "ODBC Driver 17 for SQL Server",
			Server = server,
			Database = database,
			Trusted_Connection = "Yes")
		}
		else
		{
			con <- dbConnect(odbc::odbc(),
                 Driver   = sql_driver,
                 Server   = server,
                 Database = database, 
                 UID      = user,
                 PWD      = password,
                 Port     = port)		
		}
		
		## get all tables of the database
		#tables <- dbListTables(con)
		#print(tables)
		
		##return only a few tables
		query <- "
			SELECT TABLE_SCHEMA, TABLE_NAME
			FROM INFORMATION_SCHEMA.TABLES
			WHERE TABLE_TYPE = 'BASE TABLE'
			  AND TABLE_SCHEMA = 'dbo'
			ORDER BY TABLE_NAME;
			"

		tables_df <- dbGetQuery(con, query)

		invisible(tables_df[,2])
	}
}




importMSSQLtable <- function(server="localhost", database, tablename, user=NULL, password=NULL, port=1433, WinLogin=TRUE, datasetname)
{
	library(DBI)
	library(odbc)

	con = NULL
	# List available drivers
	drivers <- odbc::odbcListDrivers()$name

	# Try to find a SQL Server driver
	sql_driver <- drivers[grepl("SQL Server", drivers, ignore.case = TRUE)][1]

	# Check if found
	if (is.na(sql_driver)) {
	  print("No SQL Server ODBC driver found. Please install 'ODBC Driver 17/18 for SQL Server'.")
	}
	else
	{
		if(WinLogin)#if windows login used
		{				 
			con <- DBI::dbConnect(odbc::odbc(),
			Driver = "ODBC Driver 17 for SQL Server",
			Server = server,
			Database = database,
			Trusted_Connection = "Yes")
		}
		else
		{
			con <- dbConnect(odbc::odbc(),
                 Driver   = sql_driver,
                 Server   = server,
                 Database = database, 
                 UID      = user,
                 PWD      = password,
                 Port     = port)		
		}
		
		if(TRUE)# (exists("con") && inherits(con, "DBIConnection") && DBI::dbIsValid(con)) 
		{
			# Load a dataset
			eval(parse(text=paste('.GlobalEnv$',datasetname,' <- dbGetQuery(con, "SELECT * FROM ', tablename,'")', sep='')))
			#datasetname = <- dbGetQuery(con, "SELECT * FROM agewt_tbl")
			print(paste('.GlobalEnv$',datasetname))
			dbDisconnect(con)
			
			#create attributes for dataset
			
		}
		invisible(paste('.GlobalEnv$',datasetname))
	}
}



