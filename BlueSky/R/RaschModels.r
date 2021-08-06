################################################################################

validateDataRasch <-function (vars, data) 
{
    y = eval(parse(text = paste(data, "[,", paste(deparse(vars), 
        collapse = ""), "]", collapse = "", sep = "")))

res = sapply(y, BSky.is.wholenumber)
# we use BSkyall to ingore NAs as we let the IRM model functions handle them
finres=sapply(as.data.frame(res), BSkyall )
if (!all(finres,na.rm=TRUE))
{
cat("One or more variables contain floating point values (values with decimal places), please re-run the analysis with integer values. You can use Data->Compute with the round function under the Math tab to round variables.")
FALSE
}
else
{


    res = sapply(y, checkDichotomous10s)
    if (all(res)) {
        TRUE
    }
    else {
        FALSE
    }
}
}



checkDichotomous10s <-function(x)
{
    y =as.character(na.omit(unique(x))) 

 # y=eval( parse( text=paste("as.character(na.omit(unique(", data, "[,", paste(deparse(vars),collapse=""), "])))", collapse="", sep="")))

   # if (y ==c("0","1") || y == c("1", "0"))
    if (identical(y, c("0","1")) || identical(y, c("1","0")))
    {
        TRUE
    }
    else
    {
         FALSE
    }
}

################################################################################


validateDataPartialCredit<- function (vars, data) 
{ 
    y = eval(parse(text = paste(data, "[,", paste(deparse(vars), 
        collapse = ""), "]", collapse = "", sep = "")))
res = sapply(y, BSky.is.wholenumber)
# we use BSkyall to ingore NAs as we let the IRM model functions handle them
finres=sapply(as.data.frame(res), BSkyall)
if (!all(finres,na.rm=TRUE))
{
cat("One or more variables contain floating point values (values with decimal places), please re-run the analysis with integer values. You can use Data->Compute with the round function under the Math tab to round variables.")
FALSE
}
else
{
    res = sapply(y, checkPolychotomous)
    if (any(res)) {
        res = sapply(y, checkLevelsStart0)
        if (all(res)) {
            TRUE
        }
        else {
            cat("All variables in a Partial Credit Model must have a level or value of 0, if using a different scale that does not start from 0, please recode your data using Data->Recode")
            FALSE
        }
    }
    else {
        cat("At least one of the variables being analyzed to create a Partial Rating Scale model must be polychotomous. If all variables are dichotomous, try running a Simple Rasch Model")
        FALSE
    }
}
}


checkPolychotomous <-function(x)
{
    y =as.character(na.omit(unique(x))) 
    # y=eval( parse( text=paste("as.character(na.omit(unique(", data, "[,",         paste(deparse(vars),collapse=""), "])))", collapse="", sep="")))

  # if (y ==c("0","1") || y == c("1", "0"))
  #  if (length(y) >2 & "0" %in% y)
    if (length(y) >2)
    {
        TRUE
    }
    else
    {
         FALSE
    }
}

checkLevelsStart0 <- function(x)
{
    y =as.character(na.omit(unique(x))) 
    # y=eval( parse( text=paste("as.character(na.omit(unique(", data, "[,",                         paste(deparse(vars),collapse=""), "])))", collapse="", sep="")))

   # if (y ==c("0","1") || y == c("1", "0"))
    if ("0" %in% y)
    {
        TRUE
    }
    else
    {
         FALSE
    }

}


validateDataRatingScale <-function (vars, data) 
{
    alllevelsequal = TRUE
    y = eval(parse(text = paste(data, "[,", paste(deparse(vars), 
        collapse = ""), "]", collapse = "", sep = "")))
    
res = sapply(y, BSky.is.wholenumber)
# we use BSkyall to ingore NAs as we let the IRM model functions handle them
finres=sapply(as.data.frame(res), BSkyall )
if (!all(finres,na.rm=TRUE))
{
cat("One or more variables contain floating point values (values with decimal places), please re-run the analysis with integer values. You can use Data->Compute with the round function under the Math tab to round variables.")
FALSE
}
else
{
    res = sapply(y, checkPolychotomous)
    if (any(res)) {
        res = sapply(y, checkLevelsStart0)
        if (all(res)) {
            count = 1
            i = 1
            res = lapply(y, unique)
            res = lapply(res, na.omit)
            res = lapply(res, sort)
            initial = res[[1]]
            for (i in 1:count) {
                if (!identical(initial, res[[i]])) {
                  alllevelsequal = FALSE
                }
            }
            if (!alllevelsequal) {
                cat("The levels of all the variables used to create the model are not identical")
            }
            TRUE
        }
        else {
            cat("All variables in a Partial Credit Model must have a level or value of 0, if using a different scale that does not start from 0, please recode your data using Data->Recode")
            FALSE
        }
    }
    else {
        cat("At least one of the variables being analyzed to create a Partial Rating Scale model must be polychotomous. If all variables are dichotomous, try running a Simple Rasch Model")
        FALSE
    }
}
}



BSkyPrintifitClass<-function (x, visible = TRUE, ...) 
{
    pvalues <- 1 - pchisq(x$i.fit, x$i.df - 1)
    coef.table <- cbind(round(x$i.fit, 3), x$i.df - 1, round(pvalues, 
        3), round(x$i.outfitMSQ, 3), round(x$i.infitMSQ, 3), 
        round(x$i.outfitZ, 2), round(x$i.infitZ, 2))
    colnames(coef.table) <- c("Chisq", "df", "p-value", "Outfit MSQ", 
        "Infit MSQ", "Outfit t", "Infit t")
    rownames(coef.table) <- names(x$i.fit)
    if (visible) {
       # cat("\nItemfit Statistics: \n")
       # print(coef.table)?
       # cat("\n")
    }
    BSkyFormat(coef.table,singleTableOutputHeader = "Itemfit Statistics" )
}

BSkySummary.tam.mml  <-function (object, file = NULL, ...)
{
    tam_osink(file = file)
    latreg <- FALSE
    if (class(object) == "tam.latreg") {
        latreg <- TRUE
        object$irtmodel <- "tam.latreg"
    }
    cat("------------------------------------------------------------\n")
    TAM:::tam_print_package_rsession(pack = "TAM")
    TAM:::tam_print_computation_time(object = object)
    cat("Multidimensional Item Response Model in TAM \n\n")
    irtmodel <- object$irtmodel
    cat("IRT Model", irtmodel)
    tam_print_call(object$CALL)
    cat("------------------------------------------------------------\n")
    cat("Number of iterations=", object$iter, "\n")
    ctr <- object$control
    if (ctr$snodes == 0) {
        cat("Numeric integration with", dim(object$theta)[1],
            "integration points\n")
    }
    if (ctr$snodes > 0) {
        if (ctr$QMC) {
            cat("Quasi Monte Carlo integration with", dim(object$theta)[1],
                "integration points\n")
        }
        if (!ctr$QMC) {
            cat("Monte Carlo integration with", dim(object$theta)[1],
                "integration points\n")
        }
    }
    cat("\nDeviance=", round(object$deviance, 2), "\n")
    cat("   Log likelihood=", round(object$ic$loglike, 2), "\n")
    cat("Number of persons=", object$nstud, "\n")
    cat("Number of persons used=", object$ic$n, "\n")
    if (!is.null(object$formulaA)) {
        cat("Number of generalized items=", object$nitems, "\n")
        cat("Number of items=", ncol(object$resp_orig), "\n")
    }
    else {
        cat("Number of items=", object$nitems, "\n")
    }
    cat("Number of estimated parameters=", object$ic$Npars, "\n")
    if (!latreg) {
        cat("    Item threshold parameters=", object$ic$Nparsxsi,
            "\n")
        cat("    Item slope parameters  =", object$ic$NparsB,
            "\n")
    }
    cat("    Regression parameters  =", object$ic$Nparsbeta,
        "\n")
    cat("    (Co)Variance parameters=", object$ic$Nparscov, "\n\n")
    res <- TAM:::tam_summary_print_ic(object = object)
    cat("------------------------------------------------------------\n")
    cat("EAP Reliability\n")
    obji <- round(object$EAP.rel, 3)
    print(obji)
    cat("------------------------------------------------------------\n")
    cat("Covariances and Variances\n")
    if (object$G > 1) {
        a1 <- stats::aggregate(object$variance, list(object$group),
            mean)
        object$variance <- a1[, 2]
    }
    obji <- round(object$variance, 3)
    if (object$G > 1) {
        names(obji) <- paste0("Group", object$groups)
    }
    print(obji)
    cat("------------------------------------------------------------\n")
    cat("Correlations and Standard Deviations (in the diagonal)\n")
    if (object$G > 1) {
        obji <- sqrt(object$variance)
    }
    else {
        obji <- stats::cov2cor(object$variance)
        diag(obji) <- sqrt(diag(object$variance))
    }
    if (object$G > 1) {
        names(obji) <- paste0("Group", object$groups)
    }
    tam_round_data_frame_print(obji = obji, digits = 3)
    cat("------------------------------------------------------------\n")
    cat("Regression Coefficients\n")
    tam_round_data_frame_print(obji = object$beta, digits = 5)
    TAM:::summary_tam_print_latreg_stand(object = object, digits_stand = 4)
    if (!latreg) {
        cat("------------------------------------------------------------\n")
        #cat("Item Parameters -A*Xsi\n")
        obji <- object$item
       temp<- BSky_tam_round_data_frame_print(obji = obji, from = 2, to = ncol(obji),
           digits = 3, rownames_null = FALSE)
       
        BSkyFormat(temp,singleTableOutputHeader="Item Parameters -A*Xsi")
        if (!is.null(object$formulaA)) {
           # cat("\nItem Facet Parameters Xsi\n")
            obji <- object$xsi.facets
            xsi99 <- sum(object$xsi == 99)
            if (xsi99 > 0) {
                cat("\nSome item xsi parameters are not estimable ")
                cat(" which is indicated by values of 99\n\n")
            }
            if (object$PSF) {
                cat("\nA pseudo facet 'psf' with zero effects with all zero effects\n")
                cat("was created because of non-unique person-facet combinations.\n\n")
            }
            temp <-BSky_tam_round_data_frame_print(obji = obji, from = 3,
                digits = 3)
             BSkyFormat(temp,singleTableOutputHeader="Item Facet Parameters Xsi")
        }
        if ((object$maxK > 2) | (object$printxsi)) {
           # cat("\nItem Parameters Xsi\n")
            obji <- object$xsi
          temp<-  BSky_tam_round_data_frame_print(obji = obji, from = 1,
                digits = 3)
             BSkyFormat(temp,singleTableOutputHeader="Item Parameters Xsi")
        }
        if (object$irtmodel %in% c("efa")) {
            cat("------------------------------------------------------------\n")
            cat("\nStandardized Factor Loadings Oblimin Rotation\n")
            print(object$efa.oblimin)
        }
        if (object$irtmodel %in% c("bifactor1", "bifactor2",
            "efa")) {
            cat("------------------------------------------------------------\n")
            if (irtmodel == "efa") {
                cat("\nStandardized Factor Loadings (Schmid Leimann transformation)\n")
                obji <- object$B.SL
            }
            else {
                cat("\nStandardized Factor Loadings (Bifactor Model)\n")
                obji <- object$B.stand
            }
            BSky_tam_round_data_frame_print(obji = obji, digits = 3)
            meas <- object$meas
            cat("\nDimensionality/Reliability Statistics\n\n")
            cat("ECV=", round(meas["ECV"], 3), "\n")
            cat("Omega Asymptotical=", round(meas["omega_a"],
                3), "\n")
            cat("Omega Total=", round(meas["omega_t"], 3), "\n")
            cat("Omega Hierarchical=", round(meas["omega_h"],
                3), "\n")
            if (object$maxK == 2) {
                cat("Omega Total (GY)=", round(meas["omega_tot_diff"],
                  3), "\n")
                cat("  Omega Total GY (Green & Yang, 2009) includes item difficulties\n")
                cat("  and estimates the reliability of the sum score.\n")
            }
        }
    }
    tam_csink(file = file)
}

BSky_tam_round_data_frame_print <-function (obji, from = 1, to = ncol(obji), digits = 3, rownames_null = FALSE)
{
    obji <- tam_round_data_frame(obji = obji, from = from, to = to,
        digits = digits, rownames_null = rownames_null)
    #print(obji)
    invisible(obji)
}


BSkySummaryeRm <-function (object, ...)
{
    #cat("\n")
    #cat("Results of", object$model, "estimation: \n")
    #cat("\n")
    #cat("Call: ", deparse(object$call), "\n")
    #cat("\n")
    cat("Conditional log-likelihood:", object$loglik, "\n")
    cat("Number of iterations:", object$iter, "\n")
    cat("Number of parameters:", object$npar, "\n")
    cat("\n")
    X <- object$X
    X01 <- object$X01
    mt_vek <- apply(X, 2, max, na.rm = TRUE)
    ci <- stats::confint(object, "eta")
    if (object$model %in% c("RM", "RSM", "PCM"))
      {
        if (is.null(object$call$W)) {
          temp <-"Item (Category) Difficulty Parameters (eta):"
        }
        else {
            temp <-paste("Item (Category) Parameters (eta):Based on design matrix W =",
               deparse(object$call$W))
            #temp <- "Item (Category) Parameters (eta):Based on design matrix W =",  deparse(object$call$W))
            }
       }
    else
    {    
    #cat("Basic Parameters eta")
    temp = "Basic Parameters eta"
    }
   
    temp = paste(temp, " with 0.95 CI:")
    coeftable <- as.data.frame(cbind(round(object$etapar, 3),
        round(object$se.eta, 3), round(ci, 3)))
    colnames(coeftable) <- c("Estimate", "Std. Error", "lower CI",
        "upper CI")
    rownames(coeftable) <- names(object$etapar)
  #  print(coeftable)
    BSkyFormat(coeftable,singleTableOutputHeader=temp)
    ci <- stats::confint(object, "beta")
  #  cat("\nItem Easiness Parameters (beta) with 0.95 CI:\n")
    coeftable <- cbind(round(object$betapar, 3), round(object$se.beta,
        3), round(ci, 3))
    colnames(coeftable) <- c("Estimate", "Std. Error", "lower CI",
        "upper CI")
    rownames(coeftable) <- names(object$betapar)
    #print(coeftable)
BSkyFormat(coeftable,singleTableOutputHeader="Item Easiness Parameters (beta) with 0.95 CI:")
    cat("\n")
}

BSkyall <- function(x)
{
all(x, na.rm=TRUE)
}


BSky.is.wholenumber <-function(x, tol = .Machine$double.eps^0.5)  
{

#if (is.na(x))
#{
#TRUE
#}
#else
#{
abs(x - round(x)) < tol
#}
}