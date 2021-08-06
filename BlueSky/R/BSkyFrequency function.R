
#Dialog editor calling syntax
# BSkyNullMucher = BSkyFrequency( vars = c("var1 name", "var2 name",...) , data = {{%DATASET%}})
# data is the dataset object itself not the string name

#This function goes into BSky R package 
BSkyFrequency <- function(vars, data) 
{
		dataset_name_str = deparse(substitute(data))
		selected_var_list = paste(paste(vars,"=",dataset_name_str,"$",vars, sep=""), sep="", collapse=",")
		
		BSky_Dataset_Overview = data.frame(Dataset = c(dataset_name_str),Variables = length(names(data)),Nominals = length(x = which(lapply(data, is.factor) == TRUE)), Observations = nrow(data))
		BSky_Variable_List = eval(parse(text= paste("list(",selected_var_list,")", sep="")))
		BSky_Summary_By_Variable <- ftable(summary(data[names(BSky_Variable_List)]))

# Old reference from dialog editor code 
#BSky_Dataset_Overview = data.frame(Dataset = c("{{%DATASET%}}"),Variables = length(names({{%DATASET%}})),Nominals = length(x = which(lapply({{%DATASET%}}, is.factor) == TRUE)), Observations = nrow({{%DATASET%}}))
#BSky_Variable_List = list({{subsetvars}})
#BSky_Summary_By_Variable <- ftable(summary({{%DATASET%}}[names(BSky_Variable_List)]))
		
#BSky_Dataset_Overview = data.frame(Dataset = c("Dataset2"),Variables = length(names(Dataset2)),Nominals = length(x = which(lapply(Dataset2,is.factor) == TRUE)),Observations = nrow(Dataset2))
#BSky_Variable_List = list(Treat = Dataset2$Treat,prewtGrp = Dataset2$prewtGrp)
#BSky_Summary_By_Variable <- ftable(summary(Dataset2[names(BSky_Variable_List)]))


		BSkyFormat(BSky_Dataset_Overview, singleTableOutputHeader=c("Dataset Overview"))
		BSkyFormat(BSky_Summary_By_Variable, singleTableOutputHeader=c("Summary By Variable"))
		#remove(BSky_Dataset_Overview)
		#remove(BSky_Summary_By_Variable)

		for (i in 1:length(BSky_Variable_List))
		{
		 freq = as.data.frame(table(BSky_Variable_List[[i]], useNA ="always"))
		 percent = as.data.frame(prop.table(table(BSky_Variable_List[[i]], useNA ="always")))
		 percent$Freq = percent$Freq*100
		 y=0
		 cumPercent = t(data.frame((lapply(percent$Freq, function(x) y <<-y+x))))

		 validFreq = as.data.frame(table(BSky_Variable_List[[i]]))
		 validPercent = as.data.frame(prop.table(table(BSky_Variable_List[[i]])))
		 validPercent$Freq = validPercent$Freq*100
		 y=0
		 validCumPercent = t(data.frame((lapply(validPercent$Freq, function(x) y <<-y+x))))

		 if(length(cumPercent) > length(validCumPercent))
		 {
		   validPercent[]= lapply(validPercent, as.character)
		   validPercent = rbind(validPercent,c("",""))
		   validCumPercent = rbind(validCumPercent,c(""))
		 }

		 BSky_freqTable = cbind(freq, percent$Freq, cumPercent, validPercent$Freq, validCumPercent)
		 names(BSky_freqTable) = c(names(BSky_Variable_List)[i], "Frequency", "Percent", "CumPercent", "Valid Percent", "Valid CumPercent")
		 row.names(BSky_freqTable) = NULL
		 tableHeader = paste("Frequency Table for", names(BSky_Variable_List)[i])
		 BSkyFormat(BSky_freqTable, singleTableOutputHeader=tableHeader)
		}
		
		#remove(BSky_Variable_List)
		#remove(BSky_freqTable)
		# to supress NULL printing as this function does not return anything 
		# y = 0
		#return(invisible(y))
}