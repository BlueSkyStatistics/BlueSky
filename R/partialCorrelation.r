BSkyPartialCorrelationsErrWarnHandler <- function(m)
{
	#print(str(m))
  if("error" %in% attr(m, "class"))
	{	
		stop(as.character(m$message))
	}
	i = uadatasets.sk$partialCorrelations$index[1]
	j = uadatasets.sk$partialCorrelations$index[2]

	#eval(parse(text="bsky_rcommand_execution_an_exception_occured = TRUE"), envir=globalenv())
	
	#print(as.character(m$message))
	uadatasets.sk$partialCorrelations$messages = c(uadatasets.sk$partialCorrelations$messages, paste(i, j,as.character(m$message)))
	
}

BSkyPartialSemiCorrelations <-function (vars, constants, type, method, data)
{
    temp = NULL
    typestring = ""
    results = NULL
    uadatasets.sk$partialCorrelations$index = NULL
    uadatasets.sk$partialCorrelations$messages = NULL
  uadatasets.sk$partialCorrelations$messages = 
        paste("Control Variables: ", base::toString(constants))
    results = matrix(rep(NA, length(vars) * 3 * (length(vars) +
        2)), nrow = length(vars) * 3, ncol = length(vars) + 2)
    results = data.frame(results)
    #results[1, 1] = paste(constants, collapse = ",")
    results[, 1] = rep(c("Estimate", "p.value", "N"),
        length(vars))
    names(results) = c( "Statistics", "Variables", vars)
    rownumber = 1
    if (type == "partial") {
        typestring = "pcor.test("
    }
    else {
        typestring = "spcor.test("
    }
    i = 1
    j = 1
    k = 1
    for (i in 1:length(vars)) {
        for (j in 1:length(vars)) {
            uadatasets.sk$partialCorrelations$index = c(rownumber,
                j)
            if (i != j) {
                tryCatch({
                  withCallingHandlers({
                    temp <- eval(parse(text = paste(typestring,
                      data, "$", vars[i], ",", data, "$", vars[j],
                      ",", data, "[,", deparse(constants), "]", ", method =", deparse(method), ")",
                      sep = "")))
                  }, warning = BSkyPartialCorrelationsErrWarnHandler,
                    silent = TRUE)
                }, error = BSkyPartialCorrelationsErrWarnHandler,
                  silent = TRUE)
            }
            else {
                temp$estimate = 1
                temp$p.value = NA
                temp$n = NA
            }
            results[rownumber, 2] = vars[i]
            results[rownumber, j + 2] = round( temp$estimate, BSkyGetDecimalDigitSetting())
            results[rownumber + 1, j + 2] = round (temp$p.value, BSkyGetDecimalDigitSetting())
            results[rownumber + 2, j + 2] = temp$n
        }
        rownumber = rownumber + 3
    }
    for (k in 1:length(uadatasets.sk$partialCorrelations$messages)) {
        attr(results, paste("BSkyFootnote_BSkyfooter", k, sep = "")) = uadatasets.sk$partialCorrelations$messages[k]
    }
  	results[is.na(results)] = c(' ')
  
  
    invisible(results)
}