### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @, percentage should be escaped using back slash)
#' @title Convert character to date
#'
#' @description Converts a character to a date (POSIXct class). You need to specify the format of the date  stored in a character string.
#' The function above internally calls strptime in the base package. We have extended strftime to support multiple variables.
#'
#' @param varNames The variable names of class character that need to be converted to date (POSIXct class)
#' @param dateFormat A character string. The default for the format methods is "\%Y-\%m-\%d \%H:\%M:\%S" if any element has a time component which is not midnight, and "\%Y-\%m-\%d" otherwise. If options("digits.secs") is set, up to the specified number of digits will be printed for seconds
#' @param timezone A character string specifying the time zone to be used for the conversion. System-specific (see as.POSIXlt), but "" is the current time zone, and "GMT" is UTC. Invalid values are most commonly treated as UTC, on some platforms with a warning.
#' @param prefixOrSuffix Specific a prefix or suffix for the converted variables of class POSIXct . Takes either c("prefix") or c("suffix"). New variables that are created with this prefix/suffix to the original variable name. 
#' @param prefixOrSuffixValue A character vector that specifies the name of the prefix or suffix to be used.
#' @param data The dataset name as a character string.
#'
#' @return a date (POSIXct class)
#'
#' @examples
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