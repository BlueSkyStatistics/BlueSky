# Sub function
#Create a driver and then creates connection to specified SQL server
ConnectSQL <- function(servertype, serveraddress, usr="", pass="", databasename="", query="", mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(databasename)

	BSkyErrMsg = paste("ConnectSQL: Error creating connection : ")
	BSkyWarnMsg = paste("ConnectSQL: Warning in creating connection : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	tryCatch(
		{
		withCallingHandlers(
		{	
			datbasenamepresent = FALSE
			executeUseStmt = FALSE
			if(nchar(databasename)>0)
			{
				cat("\nDatabase name passed for connection string.\n")
				datbasenamepresent = TRUE
			}
			
			BSkyErrMsg = paste("ConnectSQL: Error creating driver for ", servertype)
			BSkyWarnMsg = paste("ConnectSQL: Warning in creating driver for ", servertype)
			BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)				
					
			if(servertype == "MSSQL")
			{
				cat("\nCreating MSSQL Driver")
				executeUseStmt = TRUE
				require(RJDBC)
				qryResult = NULL
				#BSkySQLDriver <<- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","C:/Program Files/Microsoft SQL Server JDBC Driver 3.0/sqljdbc_3.0/enu/sqljdbc4.jar")
				BSkySQLDriver <<- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",mssqldrvjdbcpath)
			}
			else if(servertype == "MS-ACCESS")#using DBI package named RODBCDBI
			{
				cat("\nCreating MS-ACCESS Driver using DBI")
				executeUseStmt = TRUE
				require(RODBCDBI)
				require(DBI)
				qryResult = NULL
				BSkySQLDriver <<- RODBCDBI::ODBC()
			}			
			else if(servertype == "ACCESS")
			{
				cat("\nCreating MS-ACCESS Driver")
				executeUseStmt = TRUE
				require(RODBC)
				qryResult = NULL
				BSkySQLDriver = NULL
				#BSkySQLDriver <<- ("com.microsoft.sqlserver.jdbc.SQLServerDriver","C:/Program Files/Microsoft SQL Server JDBC Driver 3.0/sqljdbc_3.0/enu/sqljdbc4.jar")
			}			
			else if(servertype == "Oracle")
			{
				require(ROracle)
				BSkySQLDriver <<- dbDriver("Oracle")
				#BSkySQLconn <- dbConnect(drv, "rquser", "rquser")
			}			
			else if(servertype == "PostgreSQL")
			{
				## Found new stuff: 15May2020: (looks like old stuff has been depricated)
				## https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-step-by-step-example/
				
				## USE Databasename is not required as its connection statement does that.
				## IF you want to use different database then you need to create another connection
				## with another database name. There is no USE statement for PostgreSQL.
				require(RPostgreSQL)
				## loads the PostgreSQL driver
				BSkySQLDriver <<-  RPostgres::Postgres()##### DEPRICATED-->> dbDriver("PostgreSQL")
				## Open a connection
				#BSkySQLconn <- dbConnect(drv, dbname="R_Project")
			}
			else if(servertype == "MySQL")
			{
				executeUseStmt = TRUE
				require(RMySQL)
				BSkySQLDriver <<- MySQL()
				#BSkySQLconn <- dbConnect(MySQL(), user="network_portal", password="monkey2us", dbname=db.name, host="localhost")
			}			
			else if(servertype == "DB2")
			{
				require(RODBC)
				BSkySQLconn <<- odbcConnect(dsn=dsn.name,uid=user.name,pwd)
			}
			else if(servertype == "SQLite")
			{
				require(RSQLite)
				# SQLite only needs a path to the database. Other database drivers 
				# will require more details (like username, password, host, port etc).
				BSkySQLDriver <<- RSQLite::SQLite()
				#BSkySQLconn <- dbConnect(RSQLite::SQLite(), ":memory:")
			}
			else
			{
				cat("\nServer not yet supported!")
				bskyerrmsg =paste("ConnectSQL:Server Not supported!")
				warning("ConnectSQL: Server Not supported!")
			}
			
			
			BSkyErrMsg = paste("ConnectSQL: Error connecting to : ", servertype)
			BSkyWarnMsg = paste("ConnectSQL: Warning in connecting to : ", servertype)
			BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)	
			
			if(!is.null(BSkySQLDriver) && servertype!= "DB2") # for all other SQL servers except DB2
			{
				cat("\nCreating connection using the created driver.")
				if(servertype == "MSSQL")
				{
					cat("\nits MSSQL. Use URL")
					if(windowsAuthentication)
					{
						#semicolon not supported from C# app so it is better to use paste below.
						serveraddress = paste(serveraddress,";integratedSecurity=true",sep='')
						if(datbasenamepresent)
						{
							BSkySQLconn <<- dbConnect(BSkySQLDriver, url=serveraddress, dbname=databasename)					
						}
						else
						{
							BSkySQLconn <<- dbConnect(BSkySQLDriver, url=serveraddress)
							cat("\nMSSQL connection ready.")
						}

					}
					else
					{
						if(datbasenamepresent)
						{
							BSkySQLconn <<- dbConnect(BSkySQLDriver, url=serveraddress, user=usr, password=pass,dbname=databasename)					
						}
						else
						{
							BSkySQLconn <<- dbConnect(BSkySQLDriver, url=serveraddress, user=usr, password=pass)
							cat("\nMSSQL connection ready.")
						}
					}
				}

				else
				{
					cat("\nits non-MSSQL. Use HOST")
					if(datbasenamepresent)
					{
						cat("\nCreate SQL connection\n")
						BSkySQLconn <<- dbConnect(BSkySQLDriver, host=serveraddress, user=usr, password=pass,dbname=databasename)					
						cat("\nCreated SQL connection\n")
					}
					else
					{
						if(servertype == "PostgreSQL")
						{
							cat("\nDefault database is need for Posrtgre so using 'postgres'\n")
							#BSkySQLconn <<- dbConnect(BSkySQLDriver, host=serveraddress, user=usr, password=pass, dbname="postgres")# dbname is hardcoded in this line
							BSkySQLconn <<- dbConnect(BSkySQLDriver, host=serveraddress, user=usr, password=pass,dbname=databasename)
							cat("\nCreated SQL connection for Postgres\n")
						}
						else if(servertype=="MS-ACCESS")
						{
							#con <- dbConnect(BSkySQLDriver, dsn="test", user="sa", password="Password12!")
							cat("\nNo default databasename is needed for DBI MSACCESS\n")
							BSkySQLconn <<- dbConnect(BSkySQLDriver, dsn=serveraddress, user=usr, password=pass)
							cat("\nCreated SQL connection for non-Postgres\n")
						}
						else
						{
							cat("\nNo default databasename is needed\n")
							BSkySQLconn <<- dbConnect(BSkySQLDriver, host=serveraddress, user=usr, password=pass)
							cat("\nCreated SQL connection for non-Postgres\n")
						}
						cat("\nDriver Created: (No databasename passed)\n")
					}
				}
				if(!is.null(BSkySQLconn))
				{
					if(datbasenamepresent && executeUseStmt)
					{
						cat("\nUsing the specified table.")
						qury <- paste("USE ",databasename, sep=' ')
		tryCatch(
		{
		withCallingHandlers(
		{
						dbSendQuery(conn=BSkySQLconn, statement=qury)#The error in C# UI says something like :
						#"Unable to retrieve JDBC result set for USE bskydb (The statement did not return a result set.)"
						#which is fine because USE DATABASENAME is not supposed to return any results. It just meant to switch to
						# specified dataset. But looks like dbSendQuery or some other function is waiting for reults and thorws
						# error if no results are returned.
		},
		warning = function(w){}
		)
		},
		error=function(e){},
		silent =TRUE	
		)
					}
					# qryResult <- dbGetQuery(BSkySQLconn, query)
					# dbDisconnect(BSkySQLconn)
				}
				else
				{
					print("Cannot create SQL connection: Check connection parameters(or server).")
					BSkyErrMsg = paste("ConnectSQL: Error: executing USE statement.")
					BSkyWarnMsg = paste("ConnectSQL: Warning in executing USE statement.")
					BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
				}
				cat("\nSQL Conn created\n")
			}
			else if(servertype == "MS-ACCESS")
			{
				cat("\nits MS-ACCESS.")
				if(FALSE)#datbasenamepresent)
				{
					BSkySQLconn <<- dbConnect(dsn=serveraddress, uid=usr, pwd=pass,dbname=databasename)					
				}
				else
				{
					BSkySQLconn <<- dbConnect(dsn=serveraddress, uid=usr, pwd=pass)	#odbcConnect
					cat("\nMS-ACCESS connection ready.")
				}
			}

		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)

	    if(BSkyLocalErrorFound() == TRUE)
    	{
			cat("\nError caught in ConnectSQL \n")
    	}
		cat("\nCreated Database Driver and Connection\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
			cat("\nWarning caught in ConnectSQL \n")
			#BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		 cat("\nWrapup now")
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#return(invisible(qryResult))
}


# Sub function
# Closes SQL server connection and frees the driver.(looks like does not free the driver always)
CloseBSkySQLconnection <- function(servertype="")
{
	BSkyFunctionInit()
	BSkyErrMsg = paste("CloseBSkySQLconnection: Error closing connection : ")
	BSkyWarnMsg = paste("CloseBSkySQLconnection: Warning in closing connection : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
			withCallingHandlers(
			{
				if(servertype=="ACCESS")
				{
					#close(BSkySQLconn)
					odbcCloseAll()
				}
				else
				{
					dbDisconnect(BSkySQLconn)
				}
				BSkySQLconn <<- NULL
				#dbUnloadDriver(BSkySQLDriver)
			},
			
			warning = UAwarnHandlerFn

			) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
				silent =TRUE		
	)
	BSkyFunctionWrapUp()
}

# Sub function
#Executes any query and returns qryResults
ExecuteQuery <- function(servertype, serveraddress, usr='', pass='', databasename="", query, mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("ExecuteQuery: Error executing query : ")
	BSkyWarnMsg = paste("ExecuteQuery: Warning in executing query  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			#qryResult <- ConnectSQL(servertype, serveraddress, usr, pass, databasename)
			qryResult <- ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			if(!is.null(BSkySQLconn))
			{
				if(servertype=="MS-ACCESS")
				{
					qryResult <- sqlQuery(BSkySQLconn, query)
				}
				else
				{
					qryResult <- dbGetQuery(conn=BSkySQLconn, statement=query)
				}
				CloseBSkySQLconnection(servertype)
			}
			else
			{
				cat("\nFailed connecting to SQL server.")
			}
		},
		
			warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	

	BSkyFunctionWrapUp()
	return(qryResult)

}

# Top Level function
# Get list of databases
GetSQLDatabases <- function(servertype, serveraddress, usr='', pass='', databasename="", mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()

	BSkyErrMsg = paste("GetSQLDatabases: Error getting databasenames  : ")
	BSkyWarnMsg = paste("GetSQLDatabases: Warning in getting databasenames  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			###### table query : different for different servertypes #####
			if(servertype == "MSSQL")
			{
				cat("\nServerType=MSSQL\n")
				#get all user databases: SELECT name, database_id, create_date FROM sys.databases 
				databasequery="SELECT name FROM sys.databases"
			}
			else if(servertype == "MS-ACCESS")
			{
				cat("\nServerType=MS-ACCESS\n")
				#For ACCESS we do not have database instead we have DSN
				databasequery=""
			}			
			else if(servertype == "Oracle"){
				cat("\nServerType=Oracle\n")
			}			
			else if(servertype == "PostgreSQL")
			{
				cat("\nServerType=PosrgreSQL\n")
				#SELECT datname FROM pg_database WHERE datistemplate = false; ## list of databases
				databasequery <- "SELECT datname FROM pg_database WHERE datistemplate = false"
			}
			else if(servertype == "MySQL")
			{
				cat("\nServerType=MySQL\n")
				#Databases : SELECT table_name FROM information_schema.tables WHERE table_type = 'base table' AND table_schema='test';
				databasequery <- paste("SELECT SCHEMA_NAME AS `Database` FROM INFORMATION_SCHEMA.SCHEMATA", sep='')
			}			
			else if(servertype == "DB2")
			{
				cat("\nServerType=DB2\n")
			}
			else if(servertype == "SQLite")
			{
				cat("\nServerType=SQLite\n")
			}
			else
			{
				cat("\nDatabase query cant be generated as ServerType not yet supported!")
				bskyerrmsg =paste("ConnectSQL:Server Not supported!")
				warning("ConnectSQL: Server Not supported!")
			}
			cat('\nCreating Connection\n')
			#ConnectSQL(servertype, serveraddress, usr, pass, databasename)#databasename is required for PostgreSQL
			ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			cat('\nCreated Connection\n')
			if(!is.null(BSkySQLconn))
			{
				cat('\nExecuting Database Query\n')
				if(servertype=="MS-ACCESS")
				{
					qryResult <- data.frame(dsn=c(serveraddress))#there is no database name for ACCESS. Just DSN
				}
				else
				{
					qryResult <-  dbGetQuery(conn=BSkySQLconn, statement=databasequery) 
				}
				if(is.data.frame(qryResult) || is.matrix(qryResult))
				{
					print(class(qryResult))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = qryResult,  singleTableOutputHeader="Databases")
				}
				#print(qryResult)
				CloseBSkySQLconnection(servertype)
			}	
			else
			{
				cat("\nSQL conn si null\n")
			}
			cat('\nExecuted Database Query\n')
		},
			warning = UAwarnHandlerFn
		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	invisible( BSkyReturnStructure())#return(qryResult)	
}


# Top Level function
# Get list of Tables in a database
GetSQLTables <- function(servertype, serveraddress, usr='', pass='', databasename="", mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("GetSQLTables: Error getting tablenames  : ")
	BSkyWarnMsg = paste("GetSQLTables: Warning in getting tablenames  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			###### table query : different for different servertypes #####
			if(servertype == "MSSQL")
			{
				cat("\nServerType=MSSQL\n")
				#get all user databases: SELECT name, database_id, create_date FROM sys.databases 
				#run both q1, q2
				#tablequery <- "SELECT so.name, su.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='U'" #Owner also
				
				#tablequery <- "SELECT so.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='U'" #q2
				#tq1 <- "SELECT name FROM sysobjects where xtype='U' AND uid not in (select uid from sysusers)"  #q1
				
				#OR use just 1 query below instead of above 2 
				
				tablequery <- paste("SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.Tables WHERE TABLE_TYPE='BASE TABLE'", sep='')
				
			}
			else if(servertype == "MS-ACCESS")
			{
				cat("\nServerType=MS-ACCESS\n")
				# ConnectSQL(servertype, serveraddress, usr, pass, databasename)
				ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename)
				#accesstables <- sqlTables(BSkySQLconn, tableType='TABLE') #for RODBC
				accesstables <- dbListTables(conn=BSkySQLconn)
			
			}
			else if(servertype == "Oracle")
			{
				cat("\nServerType=Oracle\n")
			}			
			else if(servertype == "PostgreSQL")
			{
				cat("\nServerType=PostgreSQL\n")
				tablequery <- "SELECT table_name FROM information_schema.tables WHERE  TABLE_TYPE = 'BASE TABLE' AND  table_schema='public'"
			}
			else if(servertype == "MySQL")
			{
				cat("\nServerType=MySQL\n")
				tablequery <- paste("select table_name from information_schema.tables where  TABLE_TYPE = 'BASE TABLE' AND  table_schema='",databasename,"'", sep='')
			}			
			else if(servertype == "DB2")
			{
				cat("\nServerType=DB2\n")
			}
			else if(servertype == "SQLite")
			{
				cat("\nServerType=SQLite\n")
			}
			else
			{
				cat("\nTables query cant be generated as ServerType not yet supported!")
				bskyerrmsg =paste("ConnectSQL:Server Not supported!")
				warning("ConnectSQL: Server Not supported!")
			}
			cat('\nExecuting Table Query\n')
			
			#There is no need of if else as the code under if is commented. But there is no harm.
			#Just after this gets stable we will figure out if we need 'if-else' at all. Its there because of MSSQL only
			if(servertype == "MSSQL") 
			{
				#qryResult1 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				#qryResult2 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tq1)
				#Combining the results 
				#qryResult <- rbind(qryResult1, qryResult2)
				
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery, mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			}
			else if(servertype == "MS-ACCESS") 
			{
				#qryResult <- data.frame( tablenames = c(accesstables$TABLE_NAME)) #for RODBC
				qryResult <- data.frame( tablenames = c(accesstables))
			}			
			else
			{
				#qryResult <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery)
			}

			
			cat('\nExecuted Table Query\n')
			if(is.data.frame(qryResult) || is.matrix(qryResult))
			{
				print(class(qryResult))
				BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = qryResult,  singleTableOutputHeader="Tables")
			}
		},
			warning = UAwarnHandlerFn
		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	invisible( BSkyReturnStructure())#return(qryResult)	
}

# Top Level function
# Get List of Views in a database
GetSQLViews <- function(servertype, serveraddress, usr='', pass='', databasename="", mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("GetSQLViews: Error getting viewnames  : ")
	BSkyWarnMsg = paste("GetSQLViews: Warning in getting viewnames  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
		
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			
			###### views query : different for different servertypes #####
			if(servertype == "MSSQL")
			{
				cat("\nServerType=MSSQL\n")
				#get all user databases: SELECT name, database_id, create_date FROM sys.databases 
				#tablequery="SELECT so.name, su.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='V'" #owner also
				
				#tablequery="SELECT so.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='V'"
				#tq1 <- "SELECT name FROM sysobjects where xtype='V' AND uid not in (select uid from sysusers)"  #q1
				
				#OR use just 1 query below instead of above 2
				tablequery <- paste("SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.Views", sep='')
			}
			else if(servertype == "MS-ACCESS")
			{
				cat("\nServerType=MS-ACCESS\n")
				# ConnectSQL(servertype, serveraddress, usr, pass, databasename)
				ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename)
				#accesstables <- sqlTables(BSkySQLconn, tableType='TABLE') #for RODBC
				accesstables <- dbListTables(conn=BSkySQLconn)
			
			}		
			else if(servertype == "Oracle")
			{
				cat("\nServerType=Oracle\n")
			}			
			else if(servertype == "PostgreSQL")
			{
				cat("\nServerType=PostgreSQL\n")
				#SELECT datname FROM pg_database WHERE datistemplate = false; ## list of databases
				tablequery <- "SELECT table_name FROM information_schema.views WHERE table_schema='public'"
			}
			else if(servertype == "MySQL")
			{
				cat("\nServerType=MySQL\n")
				#Databases : SELECT table_name FROM information_schema.tables WHERE table_type = 'base table' AND table_schema='test';
				tablequery <- paste("select table_name from information_schema.views where table_schema='",databasename,"'", sep='')
			}			
			else if(servertype == "DB2")
			{
				cat("\nServerType=DB2\n")
			}
			else if(servertype == "SQLite")
			{
				cat("\nServerType=SQLite\n")
			}
			else
			{
				cat("\nViews query cant be generated as ServerType not yet supported!")
				bskyerrmsg =paste("ConnectSQL:Server Not supported!")
				warning("ConnectSQL: Server Not supported!")
			}			
			cat('\nExecuting Views Query\n')
			
			if(servertype == "MSSQL")
			{
				#qryResult1 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				#qryResult2 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tq1)
				#Combining the results
				#qryResult <- rbind(qryResult1, qryResult2)
				
				# qryResult <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery,mssqldrvjdbcpath)
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery,mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			}
			else if(servertype == "MS-ACCESS") 
			{
				#qryResult <- data.frame( tablenames = c(accesstables$TABLE_NAME)) #for RODBC
				qryResult <- data.frame( tablenames = c(accesstables))
			}				
			else
			{
				# qryResult <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery)
			}
			
			cat('\nExecuted Views Query\n')
			if(is.data.frame(qryResult) || is.matrix(qryResult))
			{
				print(class(qryResult))
				BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = qryResult,  singleTableOutputHeader="Views")
			}
		},
		
			warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	invisible( BSkyReturnStructure())#return(qryResult)	
}

# Top Level function
# Get list of Tables and Views
GetSQLTablesAndViews <- function(servertype, serveraddress, usr='', pass='', databasename="", mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("GetSQLTablesAndViews: Error getting table and view names  : ")
	BSkyWarnMsg = paste("GetSQLTablesAndViews: Warning in getting table and view names  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			
			###### tables and views query : different for different servertypes #####
			if(servertype == "MSSQL")
			{
				cat("\nServerType=MSSQL\n")
				#get all user databases: SELECT name, database_id, create_date FROM sys.databases 
				#tablequery="SELECT so.name, su.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='U' OR so.xtype='V'" #Owner also
				
				#tablequery="SELECT so.name FROM sysobjects so JOIN sysusers su on so.uid = su.uid where so.xtype='U' OR so.xtype='V'"
				#tq1 <- "SELECT name FROM sysobjects where xtype='V' OR xtype='U' AND uid not in (select uid from sysusers)"  #q1
				
				#OR use 1 below query below instead of above 2
				tablequery <- paste("SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.Tables Union SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.VIEWS", sep='')
				#genericQuery <- paste("SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.Tables", sep='')
				#genericQuery <- paste("SELECT TABLE_NAME, TABLE_SCHEMA FROM ",databasename,".INFORMATION_SCHEMA.Tables", sep='')
			}
			else if(servertype == "MS-ACCESS")
			{
				cat("\nServerType=MS-ACCESS\n")
				ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename)
				#accesstables <- sqlTables(BSkySQLconn, tableType='TABLE') #for RODBC
				accesstables <- dbListTables(conn=BSkySQLconn)
			
			}			
			else if(servertype == "Oracle")
			{
				cat("\nServerType=Oracle\n")
			}			
			else if(servertype == "PostgreSQL")
			{
				cat("\nServerType=PostgreSQL\n")
				#SELECT datname FROM pg_database WHERE datistemplate = false; ## list of databases
				tablequery <- "select table_name from INFORMATION_SCHEMA.tables WHERE table_schema = 'public' UNION select table_name from INFORMATION_SCHEMA.views WHERE table_schema='public'"
			}
			else if(servertype == "MySQL")
			{
				cat("\nServerType=MySQL\n")
				#Databases : SELECT table_name FROM information_schema.tables WHERE table_type = 'base table' AND table_schema='test';
				tablequery <- paste("select table_name from information_schema.tables where table_schema='",databasename,"' UNION select table_name from information_schema.views where table_schema='",databasename,"'", sep='')
			}			
			else if(servertype == "DB2")
			{
				cat("\nServerType=DB2\n")
			}
			else if(servertype == "SQLite")
			{
				cat("\nServerType=SQLite\n")
			}
			else
			{
				cat("\nTables and views query cant be generated as ServerType not yet supported!")
				bskyerrmsg =paste("ConnectSQL:Server Not supported!")
				warning("ConnectSQL: Server Not supported!")
			}			
			cat('\nExecuting Table-Views Query\n')
			
			if(servertype == "MSSQL")
			{
				#qryResult1 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				#qryResult2 <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tq1)
				#Combining the results
				#qryResult <- rbind(qryResult1, qryResult2)
				
				# qryResult <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery, mssqldrvjdbcpath)
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery,mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			}
			else if(servertype == "MS-ACCESS") 
			{
				#qryResult <- data.frame( tablenames = c(accesstables$TABLE_NAME)) #for RODBC
				qryResult <- data.frame( tablenames = c(accesstables))
			}	
			else
			{
				# qryResult <- ExecuteQuery(servertype, serveraddress, usr, pass, databasename,tablequery)
				qryResult <- ExecuteQuery(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=tablequery)
			}
			cat('\nExecuted Table-Views Query\n')
			if(is.data.frame(qryResult) || is.matrix(qryResult))
			{
				print(class(qryResult))
				BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = qryResult,  singleTableOutputHeader="Tables and Views")
			}			
		},
		
			warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	invisible( BSkyReturnStructure())#return(qryResult)	
}

# Top Level function
# Get list of columns in a Table
GetSQLTableColumns <- function(servertype, serveraddress, usr='', pass='', databasename="", tablename,mssqldrvjdbcpath="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("GetSQLTableColumns: Error getting column names  : ")
	BSkyWarnMsg = paste("GetSQLTableColumns: Warning in getting columns names  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			
	tryCatch(
	{
		withCallingHandlers(
		{
			cat("\nTrying to get table columns")
			qryResult <- NULL
			#qryResult <- ConnectSQL(servertype, serveraddress, usr, pass, databasename, mssqldrvjdbcpath)
			qryResult <- ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			cat("SQL-Connection:")
			print(is.null(BSkySQLconn))
			if(!is.null(BSkySQLconn))
			{
				cat("\nGetting Cols of \n")
				cat(tablename)
				if(servertype=="MS-ACCESS")
				{
					#ConnectSQL(servertype, serveraddress, usr, pass, databasename)
					ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename)
					#//colDetails <- sqlColumns(BSkySQLconn, sqtable=tablename)
					#//#if(colDetails)
					#//qryResult <- data.frame( columnnames = c(colDetails$COLUMN_NAME))
					
					qryResult <- dbListFields(conn=BSkySQLconn, name=tablename) #this may fail, as explained in
					##  https://www.w3schools.com/sql/sql_top.asp
					##uncomment and use following 2 lines and see if that works, if line above with 'dbListFields' fails
					#tableviewquery <- paste("select top 0 * from '",tablename,"'", sep='')
					#qryResult <- dbGetQuery(conn=BSkySQLconn, statement=tableviewquery)

				}
				else if (servertype=="PostgreSQL")
				{
					tableviewquery <- paste("select column_name from information_schema.columns where table_name = '",tablename,"'", sep='')
					qryResult <- dbGetQuery(conn=BSkySQLconn, statement=tableviewquery)
				}
				else if (servertype=="MSSQL")
				{
					# SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'News'  # I think this is advanced one.
					tableviewquery <- paste("select column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME ='",tablename,"'", sep='')
					
					# SELECT TOP 0 * FROM News  ## this does not work
					#tableviewquery <- paste("select top 0 * from [",tablename,"]", sep='')#square brackets allows minus '-' in table name
					
					qryResult <- dbGetQuery(conn=BSkySQLconn, statement=tableviewquery)
				}		
				else if (servertype=="Oracle")
				{
					tableviewquery <- paste("select column_name from '",tablename,"'", sep='')
					qryResult <- dbGetQuery(conn=BSkySQLconn, statement=tableviewquery)
				}					
				else if (servertype=="MySQL")
				{
					qryResult <- dbListFields(conn=BSkySQLconn, name=tablename)
				}
				else
				{
					tableviewquery <- paste("select column_name from '",tablename,"'", sep='')
					qryResult <- dbGetQuery(conn=BSkySQLconn, statement=tableviewquery)
				}				
				
				#for Postgres col from view are not returned by dbListFields so we can use following
				#qq <- dbGetQuery(con, "select column_name from information_schema.columns where table_name = 'prodview' ")

				
				print(class(qryResult))# "character" type
				mtrx <- as.matrix(qryResult)
				if(is.data.frame(mtrx) || is.matrix(mtrx))
				{
					print(class(mtrx))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = mtrx,  singleTableOutputHeader="Column Names")
				}				
				print(qryResult)
				CloseBSkySQLconnection(servertype)
			}
			else
			{
				cat("\nBSkySQLconn is null")
			}
		},
		
			warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	invisible( BSkyReturnStructure())#return(qryResult)

}

# Top Level function
# Get a dataframe by executing a query
GetDataframe <- function(servertype, serveraddress, usr='', pass='', databasename='', query, mssqldrvjdbcpath="", datasetname, windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("GetDataframe: Error creating dataframe  : ")
	BSkyWarnMsg = paste("GetDataframe: Warning in creating dataframe  : ")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			
	tryCatch(
	{
		withCallingHandlers(
		{
			qryResult <- NULL
			#ConnectSQL(servertype, serveraddress, usr, pass, databasename, query, mssqldrvjdbcpath)
			ConnectSQL(servertype=servertype, serveraddress=serveraddress, usr=usr, pass=pass, databasename=databasename, query=query,mssqldrvjdbcpath=mssqldrvjdbcpath, windowsAuthentication=windowsAuthentication)
			cat("\nLets now execute query to get dataframe\n")
			if(!is.null(BSkySQLconn))
			{
				cat("\nBSkySQLconn is not null\n")
				#datasetname <<- dbGetQuery(BSkySQLconn, query)
				
				{
					eval(parse(text=paste(datasetname,' <<- dbGetQuery(conn=BSkySQLconn, statement=query)' , sep='')))
					#errwarnmsg <- eval(parse(text=paste('DBI::dbGetException(BSkySQLconn)' , sep='')))
					#print(errwarnmsg)
					#if(!is.null(errwarnmsg))
					#{
						#BSkyErrMsg = paste("GetDataframe: Error executing query while creating dataframe  : ", errwarnmsg$errorMsg)
						#BSkyWarnMsg = paste("GetDataframe: Error executing query while creating dataframe  : ", errwarnmsg$errorMsg)
						#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
					#}
				}
				cat("\nds created")
				
				
				df1 <- NULL
				isdsexists <- eval(parse(text=paste('exists(datasetname)')))
				isdf <- eval(parse(text=paste('is.data.frame(',datasetname,')')))
				ismtrx <- eval(parse(text=paste('is.matrix(',datasetname,')')))
				if(isdsexists && (isdf || ismtrx))
				{
					df1 <- data.frame(A=c(TRUE))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = df1,  singleTableOutputHeader="Dataset Ready")
					####eval(parse(text=paste(qryResult,'<-', datasetname)))
					#BSky.LoadRefresh.Dataframe(datasetname)
					cat("\nDatasetN should be ready")
				}
				else
				{
					df1 <- data.frame(A=c(FALSE))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = df1,  singleTableOutputHeader="Dataset Not Ready")
					cat("\nDataset not ready for UI grid\n")
				}
				CloseBSkySQLconnection(servertype)
				cat("\nclose conn")
			}
			cat("\nDataFrame code ends\n")
		},
		
			warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
	},
		error = UAerrHandlerFn,
		silent =TRUE		
	)
	BSkyFunctionWrapUp()
	#return(qryResult)
	invisible( BSkyReturnStructure())
}

TestQuery <- function(servertype, serveraddress, usr='', pass='', databasename,  query, datasetname="", windowsAuthentication=FALSE)
{
	BSkyFunctionInit()
	tryCatch(
		{
		withCallingHandlers(
		{
	require(RJDBC)
	qryResult = NULL
	drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","C:/Program Files/Microsoft SQL Server JDBC Driver 3.0/sqljdbc_3.0/enu/sqljdbc4.jar")
	#conn <- dbConnect(drv, "jdbc:sqlserver://ANIL-FUJITSU\\SQLSERVER", "sa", "P@ssw0rd")
	BSkySQLconn <- dbConnect(drv, serveraddress, user=usr, password=pass,dbname=databasename)
	if(!is.null(BSkySQLconn))
	{
		qryResult <- dbGetQuery(conn=BSkySQLconn, statement=query)
		dbDisconnect(BSkySQLconn)
	}
},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
				silent =TRUE		
	)
			BSkyFunctionWrapUp()
	return(qryResult)
}

