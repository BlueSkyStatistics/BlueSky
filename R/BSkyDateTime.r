 BSkystrptime<-function (varNames = "", dateFormat = "", timezone = "", prefixOrSuffix = "suffix", prefixOrSuffixValue = "", data = "") 
{
    for (vars in varNames) 
	{
        if (prefixOrSuffix == "suffix") 
		{
            eval(parse(text = paste(".GlobalEnv$", data, "$", vars, prefixOrSuffixValue, "<-as.POSIXct(strptime(", ".GlobalEnv$", data, "$", vars, ",format =", deparse(dateFormat), ",tz=", deparse(timezone), "))", collapse = "", sep = "")))
        }
        else 
		{
            eval(parse(text = paste(".GlobalEnv$", data, "$", prefixOrSuffixValue,vars, "<-as.POSIXct(strptime(", ".GlobalEnv$",  data, "$", vars, ",format =", deparse(dateFormat), ",tz=", deparse(timezone), "))", collapse = "", sep = "")))
        }
    }
}

BSkystrftime <-function (varNames = "", dateFormat = "", timezone = "", prefixOrSuffix = "suffix", 
    prefixOrSuffixValue = "", data = "") 
{
    for (vars in varNames) {
        if (prefixOrSuffix == "suffix") {
            eval(parse(text = paste(".GlobalEnv$", data, "$", 
                vars, prefixOrSuffixValue, "<-strftime(", 
                ".GlobalEnv$", data, "$", vars, ",format =", 
                deparse(dateFormat), ",tz=", deparse(timezone), 
                ")", collapse = "", sep = "")))
        }
        else {
            eval(parse(text = paste(".GlobalEnv$", data, "$", 
                prefixOrSuffixValue, vars, "<-strftime(", 
                ".GlobalEnv$", data, "$", vars, ",format =", 
                deparse(dateFormat), ",tz=", deparse(timezone), 
                ")", collapse = "", sep = "")))
        }
    }
}