BSkyUnusualObs <- function (model, depVarValues, depVarName)
{
    BSkyResiduals <- stats::residuals(model)
    BSkyFitted <- stats::fitted(model)
    BSkyStdResiduals <- stats::rstandard(model)
    BSkyCooks_dist <- stats::cooks.distance(model)
    BSkyOutliers <- which(base::abs(BSkyStdResiduals) > 2)
    BSkyThreshold <- 4/length(BSkyCooks_dist)
    BSkyInfluential <- base::which(BSkyCooks_dist > BSkyThreshold)
    BSkyLeverage <- stats::hatvalues(model)
    BSkyHigh_leverage <- base::which(BSkyLeverage > 2 * mean(BSkyLeverage))
    BSkyDiagnostics <- data.frame(Observation = 1:length(BSkyResiduals),
        depVarValues, Residuals = BSkyResiduals, Std_Residuals = BSkyStdResiduals,
        Fitted = BSkyFitted, Cooks_Distance = BSkyCooks_dist,
        Leverage = BSkyLeverage)
    names(BSkyDiagnostics) = c("Observation", depVarName, "Residuals",
        "Std_Residuals", "Fitted", "Cooks_Distance", "Leverage")
    BSkyDiagnostics$Outlier <- abs(BSkyDiagnostics$Std_Residuals) >
        2
    BSkyDiagnostics$Influential <- BSkyDiagnostics$Cooks_Distance >
        BSkyThreshold
    BSkyDiagnostics$High_Leverage <- BSkyDiagnostics$Leverage >
        2 * mean(BSkyLeverage)
    BSkyDiagnostics <- BSkyDiagnostics %>% dplyr::filter(!is.na(BSkyResiduals) &
        !is.na(BSkyStdResiduals) & !is.na(BSkyFitted))
    if (nrow(BSkyDiagnostics) == 0) {
        cat("There are no unusual observations")
    }
    else {
        displayColNames = c("Observation", depVarName, "Residuals",
            "Std_Residuals", "Fitted")
        BSkyFinalOutliers <- BSkyDiagnostics %>% dplyr::filter(Outlier)      
      BSkyFinalOutliers <-BSkyFinalOutliers[,displayColNames]
      
        if (nrow(BSkyFinalOutliers) == 0) {
            cat("\nThere are no outliers.")
        }
        else {
            attr(BSkyFinalOutliers, "BSkyFootnote_BSkySplit") = "Outliers: Standardized residuals > 2"
            BSkyFormat(BSkyFinalOutliers, singleTableOutputHeader = "Outliers")
        }
        influentialNames = c("Observation", depVarName, "Residuals",
            "Std_Residuals", "Fitted")
        BSkyFinalInfluential <- BSkyDiagnostics %>% dplyr::filter(Influential)
        BSkyFinalInfluential <-    BSkyFinalInfluential[,displayColNames]
        if (nrow(BSkyFinalInfluential) == 0) {
            cat("\nThere are no influential points.")
        }
        else {
            attr(BSkyFinalInfluential, "BSkyFootnote_BSkySplit") = "Influential points: Cooks distance Relative threshold Di > 4/n"
            BSkyFormat(BSkyFinalInfluential, singleTableOutputHeader = "Influential points")
        }
        
      BSkyFinalHighLeverage <- BSkyDiagnostics %>% dplyr::filter(High_Leverage)
      
      BSkyFinalHighLeverage <- BSkyFinalHighLeverage[,displayColNames]
      
        if (nrow(BSkyFinalHighLeverage) == 0) {
            cat("\nThere are no high leverage points.")
        }
        else {
            attr(BSkyFinalHighLeverage, "BSkyFootnote_BSkySplit") = "High leverage points: > 2* mean(leverage)"
            BSkyFormat(BSkyFinalHighLeverage, singleTableOutputHeader = "High leverage points")
        }
    }
}

