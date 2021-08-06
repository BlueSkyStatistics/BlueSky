

BSkyReorderVarnamesInAlphabeticalOrder <-function (datasetName="")
{
orderedVarNames <-eval(parse(text =paste ("order(names (.GlobalEnv$", datasetName, "))",sep="")))

eval(parse(text =paste (".GlobalEnv$", datasetName ,"[,", paste (deparse(orderedVarNames), sep='', collapse=''), "]",sep='',collapse='')))
}



