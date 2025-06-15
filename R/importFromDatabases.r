
xor_deobfuscatestring <- function(encoded, key = 42) {
  obfuscated <- rawToChar(base64enc::base64decode(encoded))
  int_vals <- utf8ToInt(obfuscated)
  #rawToChar(as.raw(bitwXor(int_vals, key)))
  
  #safe_password <- "{raw_password}"  #this will support @, ;, or =
  paste("{",rawToChar(as.raw(bitwXor(int_vals, key))),"}", sep='')
}

#xor_deobfuscate("Gx0aGxwbGBkUExEYExc=", key = 42)  # Example


importMSSQLDBList <- function(server="localhost", database, user=NULL, password=NULL, port=1433, WinLogin=TRUE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("importMSSQLDBList: Error importing DBase list : ",sep="")
	BSkyWarnMsg = paste("importMSSQLDBList: Warning importing DBase list : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success = FALSE
	tables_df = NULL
	tryCatch({
	
		withCallingHandlers({
			
			library(DBI)
			library(odbc)

			if(!is.null(password) && stringr::str_trim(password)!="")
			{

				password = xor_deobfuscatestring(password, key = 42)
			}
			con = NULL
			
			# List available drivers
			drivers <- odbc::odbcListDrivers()$name

			# Try to find a SQL Server driver
			sql_driver <- drivers[grepl("Driver", drivers, ignore.case = TRUE)][1]

			# Check if found
			if (is.na(sql_driver)) {
				print("No SQL Server ODBC driver found. Please install 'ODBC Driver 17/18 for SQL Server'.")
			}
			else
			{

				if(WinLogin)#if windows login used
				{				
					con <- DBI::dbConnect(odbc::odbc(),
					Driver = sql_driver, #"ODBC Driver 17 for SQL Server", #
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
				print( (exists("con") && inherits(con, "DBIConnection") && DBI::dbIsValid(con)) )
				if(exists("con") && inherits(con, "DBIConnection") && DBI::dbIsValid(con)) 
				{
					print("Closing Dbase connection.")
					dbDisconnect(con)
				}				
			}
		}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
	}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		if(!is.null(tables_df))
		{
			invisible(tables_df[,2])
		}
		else
		{
			invisible(data.frame())
		}
}

getTableRowColCount <- function(server="localhost", database, tablename, user=NULL, password=NULL, port=1433, WinLogin=TRUE, schema_name="dbo")
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("getTableRowColCount: Error getting row/col count of the database table : ",sep="")
	BSkyWarnMsg = paste("getTableRowColCount: Warning getting row/col count of the database table : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success = FALSE
	row_count = 0
	col_count = 0
	tryCatch({
	
		withCallingHandlers({
		
			#create Connection
			library(DBI)
			library(odbc)

			if(!is.null(password) && stringr::str_trim(password)!="")
			{

				password = xor_deobfuscatestring(password, key = 42)
			}
			con = NULL
			
			# List available drivers
			drivers <- odbc::odbcListDrivers()$name

			# Try to find a SQL Server driver
			sql_driver <- drivers[grepl("Driver", drivers, ignore.case = TRUE)][1]

			# Check if found
			if (is.na(sql_driver)) {
				print("No SQL Server ODBC driver found. Please install 'ODBC Driver 17/18 for SQL Server'.")
			}
			else
			{

				if(WinLogin)#if windows login used
				{				
					con <- DBI::dbConnect(odbc::odbc(),
					Driver = sql_driver, #"ODBC Driver 17 for SQL Server", #
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
				
				## get row/col count for the table of the database
				#table_name <- "YourTable"
				#schema_name <- "dbo"

				# Get row count
				row_count_query <- sprintf("SELECT COUNT(*) AS row_count FROM [%s].[%s];", schema_name, tablename)
				row_count <- dbGetQuery(con, row_count_query)$row_count

				# Get column count
				col_count_query <- sprintf("
				  SELECT COUNT(*) AS column_count
				  FROM INFORMATION_SCHEMA.COLUMNS
				  WHERE TABLE_NAME = '%s' AND TABLE_SCHEMA = '%s';", tablename, schema_name)
				col_count <- dbGetQuery(con, col_count_query)$column_count

				# Display
				#cat(sprintf("Table [%s.%s] has %d rows and %d columns.\n", schema_name, tablename, row_count, col_count))				

				if(exists("con") && inherits(con, "DBIConnection") && DBI::dbIsValid(con)) 
				{
					print("Closing Dbase connection..")
					dbDisconnect(con)
				}				
			}		

		
		
		}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		#invisible(list(row_count = row_count,column_count = col_count)) #print(dims$row_count) print(dims$column_count)
		
		invisible(paste(row_count,':',col_count, sep=''))
}


importMSSQLtable <- function(server="localhost", database, tablename, user=NULL, password=NULL, port=1433, WinLogin=TRUE, datasetname)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("importMSSQLtable: Error importing DBase table : ",sep="")
	BSkyWarnMsg = paste("importMSSQLtable: Warning importing DBase table : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success = FALSE

	tryCatch({
	
		withCallingHandlers({
			library(DBI)
			library(odbc)
			if(!is.null(password) && stringr::str_trim(password)!="")
			{
				password = xor_deobfuscatestring(password, key = 42)
			}
			con = NULL
			# List available drivers
			drivers <- odbc::odbcListDrivers()$name

			# Try to find a SQL Server driver
			sql_driver <- drivers[grepl("Driver", drivers, ignore.case = TRUE)][1]
			#matched <- drivers$name[grepl("SQL Server", drivers$name, ignore.case = TRUE)]
			
			# Check if found
			if (is.na(sql_driver)) {
				print("No SQL Server ODBC driver found. Please install 'ODBC Driver 17/18 for SQL Server'.")
			}
			else
			{
				if(WinLogin)#if windows login used
				{				 
					con <- DBI::dbConnect(odbc::odbc(),
					Driver = sql_driver, #"ODBC Driver 17 for SQL Server",
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
				
				#query = paste("SELECT * FROM ', tablename,'", sep='') #you can execute query provided by user but
				# sanitize it otherwise user may send some delete type of query.
				
				if(exists("con") && inherits(con, "DBIConnection") && DBI::dbIsValid(con)) 
				{
					# Load a dataset
					eval(parse(text=paste('.GlobalEnv$',datasetname,' <- dbGetQuery(con, "SELECT * FROM ', tablename,'")', sep='')))
					#datasetname = <- dbGetQuery(con, "SELECT * FROM agewt_tbl")
					#print(paste('.GlobalEnv$',datasetname))
					print("Closing Dbase connection...")
					dbDisconnect(con)
				}
			}
		
		}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)		
		invisible()
}



