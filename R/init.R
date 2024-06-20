######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# .onLoad
#################################################################################################
BSkySQLconn <<- NULL
BSkySQLDriver <<-NULL
uadatasets <-NULL
ualog <-NULL
uaperformance =1
bskymaxfactors = 20
uadatasets.sk <- NULL
BSkyBlankDSColNameClassList <<- list()
BSkyBlankDSColNameLevelsList <<- list()
BSkyblankDSallColAttr <<- list()
blankDScolumnnames <<- NULL
blankDSrowcount <<- NULL
blankDScolremoved <<- 0
#bskyCurrentDatasetSplitSliceObj <- NULL
.onLoad<-function(libname, pkgname)
{
require(data.table)
require(kableExtra)
require(dplyr)
 #cat("Initializing the package in .onLoad function\n")
 #print(libname)
 #print(pkgname) 
 #uaOpenDataSetPackageEnv <<- new.env(parent=globalenv())
uadatasets <-NULL

#environment(.onLoad)$bskyCurrentDatasetSplitSliceObj <- NULL
assign("bskyCurrentDatasetSplitSliceObj", NULL, environment() )
ualog <-NULL
uaperformance =1
 uadatasets <<- new.env(parent=environment(.onLoad))
 uadatasets$lst<<-list()
 uadatasets$warnmsg <<-NULL
 ualog <<-new.env(parent=environment(.onLoad))
 uadatasets$index <<-numeric()
 uadatasets$index <<- 1
 # A single function call may generate several error messages, all the warning messages and calling functions must be returned
 uadatasets$warnindex <<-numeric()
 uadatasets$warnindex <<-0
 uadatasets$warnmsg <<-character()
 uadatasets$warnmsg <<-NULL
 uadatasets$errmsg <<-character()
 uadatasets$errmsg <<-NULL
 uadatasets$warning <<-0
uadatasets$error <<-0
uadatasets$errorindex<<-0
uadatasets$errorfn<<-0
uadatasets$uawarnfn<<-0
uadatasets$uawarnvar<<-NULL
uadatasets$uawarnmsgdis <<-NULL
uadatasets$split <<-FALSE
uadatasets$logindex<<-1
uadatasets.sk <<- new.env(parent=environment(.onLoad))
BSKY.replaceSplChars <<- FALSE
uadatasets.sk$BSkySigfColPatterns =c("Pr(>F)", "Pr(>|t|)","p.value", "Sig.", "Sig.(2-tailed)", "p-value", "Pr(>|z|)", "Pr(>Chi)", "p.value(z)", "p.value(t)", "Sig.(2-tail)", "Sig.(1-tail, >)", "Sig.(1-tail, <)", "Pr(>Chisq)", "P(>|Chi|)", "Pr(Chi)")
}


































uagetdatasetsenv <-function()
{
    #assignment of environment variable does not make a copy - assignments are truly pointers
    return(uadatasets)
}

uagetlogenv <-function()
{
	return(ualog)
}



	