test_that("BSkyCorrelationMatrix resolves rcorr.adjust without RcmdrMisc attached", {
  skip_if_not_installed("RcmdrMisc")

  # BlueSky never attaches RcmdrMisc, so the correlation code must reach
  # rcorr.adjust() through RcmdrMisc:: rather than by bare name; recreate a
  # session with RcmdrMisc off the search path and confirm the call resolves.
  was_attached <- "package:RcmdrMisc" %in% search()
  if (was_attached) detach("package:RcmdrMisc", character.only = TRUE, unload = FALSE)
  on.exit(if (was_attached) suppressPackageStartupMessages(library(RcmdrMisc)), add = TRUE)

  err <- tryCatch({
    BSkyCorrelationMatrix(mtcars, vars = c("mpg", "hp", "disp"))
    NULL
  }, error = function(e) conditionMessage(e))

  expect_false(
    !is.null(err) && grepl("could not find function .rcorr.adjust.", err),
    info = err
  )
})
