
#There are 2 types of tables returned by analytical functions normal tables and crosstabs (cross tabulations)

#Normal tables
#For normal tables, there is an entry in the metadata table for every error, warning or footer associated with the data table.
#The entry in the metadata table lists the rows and columns in the data table that is associated with the error, warning and footer.
#In the application, Errors and warnings can be displayed above the generated table in a text box, however it is possiable to 
#display errors and warnings in the footer in addition to the text box above the table.
#Informational messages can also be displayed in a footer.
#Superscripts are automatically associated with footers. Superscripts are placed in the footer as well as in the datacells that the
#footer corresponds to.
#The number or letters associated with the superscripts are handled by the application.
#See rendering tables of type "normal" for more details

#########################################################################################
#Rendering tables of type "normal"
#########################################################################################
# The dataframe(structure) that is used to define metadata that describes how a normal table should be rendered has 8 columns. 
#The names and the types of the columns are listed below
#varIndex (numeric)
#type (numeric): 
#varName (string)
#dataTableRow (numeric)
#startCol (numeric)
#endCol (numeric)
#BSkyMsg (string)
#RMsg (string)

#1st variable in the dataframe (structure used to define metadata for table rendering) is the index of the variable that generates the error, warning
#or footer. The name of this variable in the dataframe (structure used to define metadata for table rendering) used for storing the index is 
#varIndex.

#2nd variable in the dataframe (structure used to define metadata for table rendering) is the type of message.  If type is -1, it 
#indicates an error. If type is -2, it indicates a critical error. If type is 1, it indicates a warning. If type is 2, it indicates 
#a footer. 
#The name of the variable in the dataframe (structure used to define metadata for table rendering) used to store the type of the message is type.
#There is also the possibility that we want to generate a warning and have the warning message go into the footer, this will be indicated 
#by storing 3 in type, for an error message that will be stored in the footer, we will store 4 in type.

#We need to define what a critical error is. Is it an empty dataset vs. just a statistical function failing like a t.test fails while
# the descriptives are OK



#3rd variable in the dataframe (structure used to define metadata for table rendering) is the variable name(s) associated with the error, warning or 
#footer. The name of this variable in the dataframe (structure used to define metadata for table rendering)used for storing the variable name(s)
#is varName.
#WHEN THERE ARE MORE THAN ONE VARIABLE INVOLVED IN THE TEST GENERATING THE ERROR, WARNING OR  FOOTER THESE VARIABLES SHOULD BE MADE
#INTO A SINGLE STRING. USE THE FUNCTION uavectortostringnew AND SEE K INDEPENDENT SAMPLES AND K RELATED SAMPLES.R

#4th variable in the dataframe (structure used to define metadata for table rendering) is the row number in the data table that the error, warning or 
#footer is associated with (This is optional and available only when #there is a footer). The name of this variable in the 
#dataframe (structure used to define metadata for table rendering)used for storing the row number is dataTableRow.
	
#5rd variable in the dataframe (structure used to define metadata for table rendering)is the starting column in the data table that the error, warning 
#or footer is associated with. The name of this variable in the dataframe (structure used to define metadata for table rendering)used for storing the 
#starting column is startCol.


#6th variable in the dataframe (structure used to define metadata for table rendering)is the ending column in the data table that the error, warning 
#or footer is associated with. The name of this variable in the dataframe (structure used to define metadata for table rendering)used for storing the 
#ending column is endCol.

#7th variable in the dataframe (structure used to define metadata for table rendering)is the BSky warning message, the footer or the error message 
#that should be displayed. The name of this variable in the dataframe (structure used to define metadata for table rendering)used for storing the 
#BSky message is BSkyMsg.


#8th variable in the dataframe (structure used to define metadata for table rendering)is the message from R that is associated with the error, warning
#or footer.(This is optional). The name of this variable in the dataframe (structure used to define metadata for table rendering)used for storing the
#R message is RMsg.

#Exception 1 --NOT SURE I AGREE WITH THIS. PLEASE TEST
#In the case of 1 Sample KS, we are calling 1 sample KS against each variable for every different distribution
#There is a need to store and display the distribution number as it corresponds to the type of distribution that we are testing against
# Uamatdis looks like
#1st position is the iteration
#2st position is the index of the variable in uavarindex
#3nd position is whether everything is OK (1) (This is depricated, Added by Aaron 08/17/2013 in Costa Rica), its an error(-1) or warning(-2) or footer (2)
#4rd position is the variable name
#5th position is the warning message that should be displayed
#6th position is the R warning message

###############################################################################################


#CROSS TAB tables

# BEFORE YOU PROCESS THE METADATA AND DATA TABLES ASSOCIATED WITH THE CROSSTAB YOU NEED TO KNOW THE INFORMATION BELOW 
#Before you process my data and meta-data tables you(Sanjays program)  need to know the following
#	The row and column variables
#	The options selected
#	The number of levels in the row and column variables
#	From  1to 3, you must be able to compute the number of rows you expect when you cross all levels of the row level with all levels of the column variable (WE CALL THIS A TUPLE)
#	So if store has 5 levels and you have selected the following below, you will expect 21 rows (PER TUPLE), including the total. This is computed as follows, 4 rows (count , residual, standard residual, adjusted residual ) *5 (store1, #	stoe2, store3, store4, store5)=20+1 (total)
# 	chisq=TRUE,
# 	mcnemar=TRUE,
#  	fisher=FALSE,
#                        prop.r=FALSE,
#                        prop.c=FALSE,
#                        resid=TRUE,
#                        sresid=TRUE,
#                       asresid=TRUE,
#                        expected=FALSE,
#	Only when you know the above can you make sense of the 2 meta-data table
#	There is some other complexity, if one of the column levels are all 0's, you need to omit that column, I tell you the column levels in the $tables[[1]]$columnNames
#	Note that we always suppress all rows with 0 counts and the corresponding stats associated with these rows e.g. residuals, standard residual, adjusted residual. That information is in meta-data table2
#
#	Metadata table one has no value
#For crosstab tables for every data table there are 3 metadata tables
#The first metadata table contains an entry for every potential row in the sub-table generated for a non-empty layer tuple (A 
# non empty tuple is a tuple of the layers where all counts are not zero or one or more of the levels in a layer 
#are not dropped because that level is unused through out the dataset.)
#Please see the example below for a description of a tuple, sub-table and dropped level
# The entry describes whether that row should be displayed in the sub-table or not. 
# For every crosstab, the number of entries is constant and depends on the number of levels in the row and column variables and
# options selected like row, column percents and expected cell counts.
#The 2nd metadata table lists all non empty layer tuples.
# The 3rd meta-data table lists all the errors and warnings

#Lets illustrate this with an example. Consider the following variables
#store has 5 levels: store1, store2, store3, store4,store5
#overall has 5 levels: strongly negative, negative, neutral, somewhat positive, strongly positive
#gender has 2 levels: male, female
#contact has 2 levels: yes and no
#We are generating a crosstab with store as the row variable and overall as the column variable, gender and contact are in the layers.
#A tuple of the layers is a unique combination of levels in gender and contact. So, the tuples would be
#gender =male, contact =yes
#gender =male, contact =no
#gender =female, contact =yes
#gender=female, contact =no
#A dropped level is a level for which the value is not represented in the data.

#See rendering tables of type crosstab for more details


###############################################################################################
#Rendering tables of type "crosstab"
###############################################################################################
#There are 3 metadata tables that describe how a crosstab is rendered

#The first metadata table contains an entry for every potential row in the sub-table generated for a non-empty layer tuple (A 
# non empty tuple is a tuple of the layers where all counts are not zero or one or more of the levels in a layer 
#are not dropped because that level is unused through out the dataset.)
#The first metadata table is a matrix with 2 columns
#The the first column is the row number in the sub-table, the second column tells us whether to display the row, 0=don't display, 1 
#=display.

#The second meta-data table lists the non empty layer tuples. It is a matrix, There is a column for every variable in the layers.
#The columns hold the levels for that layer variable for which a non empty crosstab sub-table is generated

###############################################################################################

###############################################################################################





#################################################################################
#Guidelines for the return structure for functions written in the BSky application 
#################################################################################
#The 1st return value tells us whether there is a split or not. The object name is split. 0=no split, 1 =split
#The 2nd return value tells us whether there is an error or not. The object name is error. 0=no errors, -1 means there are errors
#The 3rd return value tells us how many errors there are. The object name is nooferrors.
#The 4rd return value tells us how many warnings there are. The object name is noofwarnings
#The 5th return value gives us all the log values. The object name is log
#The 6th return value gives us the summary of the function. The object name is summary.
#The 7th return value gives us the number of tables we need to display. The object name is nooftables.
#The 8th return value onwards returns a list for every table or graph we need to display
 
#Each list contains the following elements
#1-the type, this is either table or graph. The object name is type
#2 -metadata, this is either yes or no. This tells us whether there is an accompanying meta data table. The object name is metadata
#3 -The number of meta data tables. The object name is nometadatatables
#4 -The type of the meta data tables. There is a type associated with each meta data table returned.Currently we support
# normal and crosstab types. The object name is metadatatabletype. When there is more than one metadata table, the list metadatatabletype
#contains an element for each metadata table.
#5 -metadatatable, this is a list containing all the meta data tables. The tables must be listed according to the type 
#specified in 4. The object name is metadatatable
#metadatatable[[1]] contains the first metadata table
#metadatatable[[2]] contains the 2nd metadata table
#6 -datatable, this contains the data. The object name is datatable
#7 -cartlevel, If there are splits, this gives us the value of the levels we are splitting by. If there are no splits,
# this object is not returned. Used for trouble shooting purposes. The object name is cartlevel.
#8 -varindex, if there are splits, this tells us the variables involved in the split. Used for trouble shooting purposes. The object name is
#varindex

#Defining the structure of the normal table type
#With normal tables there is 1 metadata table for every data table
#The metadata table contains errors, warnings and footers and maps to the data table.
#The structure of the metadata table is described in the section

