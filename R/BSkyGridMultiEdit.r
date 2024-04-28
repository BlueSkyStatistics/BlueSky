BSkyMultipleEditDataGrid <-function(startRow =2, startCol =2, noOfRows =3, noOfCols=4,
                          data = c( 6,2,3,4,8,6,7,8,6,10,11,12 ),dataSetNameOrIndex ="mtcars" )
{
#lets get each column
  for (i in 1:noOfCols)
  {
# Get every 4th element
every_column <- data[seq(i, length(data), by = noOfCols)]
# Print the result
#print(every_column)
    #Lets replace values in the dataframe
    #Lets get the corresponding class of the column that needs to be replaced
    classOfVariable  <- eval(parse(text = paste("class(",
                  dataSetNameOrIndex, "[,",startCol,"])")))
    print(classOfVariable)  
    if ("numeric" %in% classOfVariable ||"integer" %in% classOfVariable )
    {
     #tryCatch( expr = { every_column = as.numeric(every_column)}, error = function (e)
     #{
          # Code to handle the error
      #    cat("An error occurred: ", conditionMessage(e), "\n")
          # You can also choose to return a default value or take other actions
       #   return(NULL)
     #})
      print(every_column)
      every_column_temp <-every_column
      empty_string_count <- sum(nchar(every_column) == 0)
      every_column = suppressWarnings(as.numeric(every_column))
      empty_numeric_count <-sum(is.na(every_column) == 0)
      if (empty_string_count != empty_numeric_count)
     {
        every_column <-every_column_temp
        # One or more of the pasted values cannot be converted to numerics
        # Lets convert the destination variable to a character
      eval(parse(text=paste(".GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]", "<- as.character(.GlobalEnv$" ,dataSetNameOrIndex, "[, " , startCol, "])", sep='')))
     
      }
     
      # Determine the end position for replacement
      end_position <- startRow +  noOfRows - 1
      # Replace part of the vector
      eval(parse(text = paste(".GlobalEnv$",
                  dataSetNameOrIndex, "[startRow:end_position, " ,startCol, "] <- every_column", sep='')))    
      startCol = startCol +1
    } else if ("factor" %in% classOfVariable || "ordered" %in% classOfVariable )
    {
     #i=2
      #every_column = c(1,4,7,10)
      every_column = as.factor(every_column)
     levelsInPastedData = levels(every_column)
      #cat("\nlevelsInPastedData:",levelsInPastedData)
     
     levelsInDestinationColumn = eval(parse(text = paste("levels(",
                  dataSetNameOrIndex, "[,", startCol, "])", sep='')))
      #cat("\nlevelsInDestinationColumn:",levelsInDestinationColumn)
     for( element in levelsInPastedData) {
        print(element)
  if (!(element %in% levelsInDestinationColumn)){
       # cat("\nValue of element:", element )
       # cat("\nValue of levelsInDestinationColumn", levelsInDestinationColumn)
       
        #print(parse(text =paste(".GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]", " <- factor(.GlobalEnv$",
         #     dataSetNameOrIndex,  "[,", startCol, "],",
#"levels = c(levels(.GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]),",  deparse(element)  ,"))", sep='')))
       
          eval(parse(text =paste(".GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]", " <- factor(.GlobalEnv$",
              dataSetNameOrIndex,  "[,", startCol, "],",
"levels = c(levels(.GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]),",  deparse(element)  ,"))", sep='')))
         }    
}
     # Determine the end position for replacement
      end_position <- startRow + noOfRows - 1
      # Replace part of the vector
      eval(parse(text = paste(".GlobalEnv$",
                 dataSetNameOrIndex, "[startRow:end_position, " ,startCol, "] <- every_column", sep='')))      
      startCol = startCol +1
    } else if ("character" %in% classOfVariable ){
     
      # Determine the end position for replacement
      end_position <- startRow +  noOfRows - 1
      # Replace part of the vector
      eval(parse(text = paste(".GlobalEnv$",
                  dataSetNameOrIndex, "[startRow:end_position, " ,startCol, "] <- every_column", sep='')))    
      startCol = startCol +1
     
    } else if ("Date" %in% classOfVariable || "POSIXct" %in% classOfVariable || "logical" %in% classOfVariable)
      {
     
      # Lets convert the destination variable to a character
      eval(parse(text=paste(".GlobalEnv$", dataSetNameOrIndex,  "[,", startCol, "]", "<- as.character(.GlobalEnv$" ,dataSetNameOrIndex, "[, " , startCol, "])", sep='')))
      # Determine the end position for replacement
      end_position <- startRow +  noOfRows - 1
      # Replace part of the vector
      eval(parse(text = paste(".GlobalEnv$",
                  dataSetNameOrIndex, "[startRow:end_position, " ,startCol, "] <- every_column", sep='')))    
      startCol = startCol +1
    }
   }
}