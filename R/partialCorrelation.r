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

BSkyPartialSemiCorrelations <-
function (vars, constants, type, data)
{
    temp=NULL
	typestring=""
   results = NULL
    uadatasets.sk$partialCorrelations$index = NULL
    uadatasets.sk$partialCorrelations$messages = NULL
    results = matrix(rep(NA, length(vars) * 3 * (length(vars) +
        3)), nrow = length(vars) * 3, ncol = length(vars) + 3)
    results = data.frame(results)
    results[1, 1] = paste(vars, collapse = ",")
    results[, 2] = rep(c("Estimate", "p.value", "Test statistic"),
        length(vars))
    names(results) = c("Control Variables", "", "", vars)
    rownumber = 1
    if (type == "partial") {
        typestring = "pcor.test("
    } else {
        typestring = "spcor.test("
    }
  i=1
  j=1
  k=1
  for (i in 1:length(vars)) {
   
        for (j in 1:length(vars)) {
            uadatasets.sk$partialCorrelations$index = c(rownumber,
                j)
          if ( i != j)
            {
              tryCatch({
                  withCallingHandlers({
                    temp <- eval(parse(text = paste(typestring,
                      data, "$", vars[i], ",", data, "$", vars[j],
                      ",", data, "[,", deparse(constants), "])",
                      sep = "")))
                  }, warning = BSkyPartialCorrelationsErrWarnHandler,
                    silent = TRUE)
              }, error = BSkyPartialCorrelationsErrWarnHandler,
                  silent = TRUE)
            } else {
              temp$estimate =1
              temp$p.value =NA
              temp$statistic =NA
             
            }
            results[rownumber, 3] = vars[i]
            results[rownumber, j+3] = temp$estimate
            results[rownumber + 1, j+3] = temp$p.value
            results[rownumber + 2, j+3] = temp$statistic
            #rownumber = rownumber + 3
        }
    rownumber = rownumber+ 3
    }
    for (k in 1:length(uadatasets.sk$partialCorrelations$messages)) {
        attr(results, paste("BSkyFootnote_BSkyfooter", k, sep = "")) = uadatasets.sk$partialCorrelations$messages[k]
    }
    invisible(results)
}