BSkyUnusualObs <- function(model, depVarValues, depVarName)
{
  # Residuals
  BSkyResiduals <- stats::residuals(model)
  # Fitted
  BSkyFitted <- stats::fitted(model)
  # Standardized residuals
  BSkyStdResiduals <- stats::rstandard(model)
  #Cook's distance
  BSkyCooks_dist <- stats::cooks.distance(model)

  # Identify observations with large residuals (e.g., > 2 or < -2)
  ##Outliers: Standardized residuals > 2
  BSkyOutliers <- which(base::abs(BSkyStdResiduals) > 2)

  # Identify influential points (Cook's Distance > threshold)
  BSkyThreshold <- 4 / length(BSkyCooks_dist)
  BSkyInfluential <- base::which(BSkyCooks_dist > BSkyThreshold)

  # Leverage
  BSkyLeverage <- stats::hatvalues(model)

  # Identify high-leverage points
  BSkyHigh_leverage <- base::which(BSkyLeverage > 2 * mean(BSkyLeverage))
  ## 

  # Combine diagnostics into a table
  BSkyDiagnostics <- data.frame(
    Observation = 1:length(BSkyResiduals),
	depVarValues,
    Residuals = BSkyResiduals,
    Std_Residuals = BSkyStdResiduals,
    Fitted = BSkyFitted,
    Cooks_Distance = BSkyCooks_dist,
    Leverage = BSkyLeverage
  )

	names(BSkyDiagnostics) = c("Observation", depVarName,"Residuals","Std_Residuals","Fitted", "Cooks_Distance","Leverage")
  # Flag unusual observations
  BSkyDiagnostics$Outlier <- abs(BSkyDiagnostics$Std_Residuals) > 2
  BSkyDiagnostics$Influential <- BSkyDiagnostics$Cooks_Distance > 	BSkyThreshold
  BSkyDiagnostics$High_Leverage <- BSkyDiagnostics$Leverage > 2 * mean(BSkyLeverage)

  #The code below ensures that rows with non-NA values in all three columns (BSkyResiduals, BSkyStdResiduals, BSkyFitted) are retained.
  #filter() is used to subset rows.
  #!is.na() checks for non-NA values in each specified column.
  #The & operator ensures that rows with non-NA values in all three columns (BSkyResiduals, BSkyStdResiduals, BSkyFitted) are retained.
  BSkyDiagnostics <- BSkyDiagnostics %>%
    filter(!is.na(BSkyResiduals) & !is.na(BSkyStdResiduals) & !is.na(BSkyFitted))
  
  if (nrow(BSkyDiagnostics)==0)
    {
    cat("There are no unusual observations")
  } else
  {
  #Display outliers
  displayColNames = c("Observation", depVarName,"Residuals","Std_Residuals","Fitted")
  BSkyFinalOutliers <- BSkyDiagnostics %>%
    filter(Outlier) %>% select(all_of(displayColNames))
    if (nrow(BSkyFinalOutliers) ==0)
      {
      cat("\nThere are no outliers.")
      } else{
  attr(BSkyFinalOutliers, "BSkyFootnote_BSkySplit") = "Outliers: Standardized residuals > 2"
  BSkyFormat(BSkyFinalOutliers, singleTableOutputHeader="Outliers")
      }

  #Display influential points
  influentialNames = c("Observation", depVarName,"Residuals","Std_Residuals","Fitted")
  BSkyFinalInfluential <- BSkyDiagnostics %>%
    filter(Influential) %>% select(all_of(displayColNames))
    if (nrow(BSkyFinalInfluential) ==0)
      {
      cat("\nThere are no influential points.")
      } else{
  attr(BSkyFinalInfluential, "BSkyFootnote_BSkySplit") = "Influential points: Cooks distance Relative threshold Di > 4/n"
  BSkyFormat(BSkyFinalInfluential, singleTableOutputHeader="Influential points")
      }

  #Display high leverage points
    
  BSkyFinalHighLeverage <- BSkyDiagnostics %>%
    filter(High_Leverage) %>% select(all_of(displayColNames))
    if (nrow(BSkyFinalHighLeverage) ==0)
      {
      cat("\nThere are no high leverage points.")
      } else{
  attr(BSkyFinalHighLeverage, "BSkyFootnote_BSkySplit") = "High leverage points: > 2* mean(leverage)"
  BSkyFormat(BSkyFinalHighLeverage, singleTableOutputHeader="High leverage points")
  }
    }
}