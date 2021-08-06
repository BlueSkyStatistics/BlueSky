BSky.Sample.Dataset <-function(originalDataset, size, replace = FALSE, newdatasetname)
{
#sample(x, size, replace = FALSE)
eval(parse(text=paste('.GlobalEnv$',newdatasetname,' <- .GlobalEnv$',originalDataset,'[base::sample(nrow(.GlobalEnv$',originalDataset,'), size, replace=replace), ]', sep='' ) ) )
#print("Dataset Created")
}
