lav_data_print_short_aar <- function (object, nd = 3L)
{
    if (inherits(object, "lavaan")) {
        object <- lav_data_summary_short(object)
    }
    datasummary <- object
    num.format <- paste("%", max(8L, nd + 5L), ".", nd, "f",
        sep = "")
    threecolumn <- !is.null(datasummary$norig)
    multilevel <- !is.null(datasummary$nlevels)
    clustered <- !is.null(datasummary$cluster) && is.null(datasummary$nlevels)
    c1 <- c2 <- c3 <- character(0L)
    if (datasummary$ngroups == 1L) {
        if (threecolumn) {
            c1 <- c(c1, "")
            c2 <- c(c2, "Used")
            c3 <- c(c3, "Total")
        }
        c1 <- c(c1, "Number of observations")
        c2 <- c(c2, datasummary$nobs)
        c3 <- c(c3, ifelse(threecolumn, datasummary$norig, ""))
    }
    else {
        c1 <- c(c1, "Number of observations per group:")
        if (threecolumn) {
            c2 <- c(c2, "Used")
            c3 <- c(c3, "Total")
        }
        else {
            c2 <- c(c2, "")
            c3 <- c(c3, "")
        }
        for (g in 1:datasummary$ngroups) {
            c1 <- c(c1, sprintf("  %-40s", datasummary$group.label[g]))
            c2 <- c(c2, datasummary$nobs[g])
            c3 <- c(c3, ifelse(threecolumn, datasummary$norig[g],
                ""))
        }
    }
    if (datasummary$ngroups == 1L) {
        if (multilevel) {
            for (l in 2:datasummary$nlevels) {
                c1 <- c(c1, paste("Number of clusters [", datasummary$cluster[l -
                  1], "]", sep = ""))
                c2 <- c(c2, datasummary$nclusters[l])
                c3 <- c(c3, "")
            }
        }
        else if (clustered) {
            c1 <- c(c1, paste("Number of clusters [", datasummary$cluster,
                "]", sep = ""))
            c2 <- c(c2, datasummary$nclusters[2])
            c3 <- c(c3, "")
        }
    }
    else {
        if (multilevel) {
            for (l in 2:datasummary$nlevels) {
                c1 <- c(c1, paste("Number of clusters [", datasummary$cluster[l -
                  1], "]:", sep = ""))
                c2 <- c(c2, "")
                c3 <- c(c3, "")
                for (g in 1:datasummary$ngroups) {
                  c1 <- c(c1, sprintf("  %-40s", datasummary$group.label[g]))
                  c2 <- c(c2, datasummary$nclusters[[g]][l])
                  c3 <- c(c3, "")
                }
            }
        }
        else if (clustered) {
            c1 <- c(c1, paste("Number of clusters [", datasummary$cluster,
                "]:", sep = ""))
            c2 <- c(c2, "")
            c3 <- c(c3, "")
            for (g in 1:datasummary$ngroups) {
                c1 <- c(c1, sprintf("  %-40s", datasummary$group.label[g]))
                c2 <- c(c2, datasummary$nclusters[[g]][2])
                c3 <- c(c3, "")
            }
        }
    }
    if (!is.null(datasummary$npatterns)) {
        if (datasummary$ngroups == 1L) {
            if (multilevel) {
                c1 <- c(c1, "Number of missing patterns -- level 1")
                c2 <- c(c2, datasummary$npatterns)
                c3 <- c(c3, "")
                if (!is.null(datasummary$npatterns2)) {
                  c1 <- c(c1, "Number of missing patterns -- level 2")
                  c2 <- c(c2, datasummary$npatterns2)
                  c3 <- c(c3, "")
                }
            }
            else {
                c1 <- c(c1, "Number of missing patterns")
                c2 <- c(c2, datasummary$npatterns)
                c3 <- c(c3, "")
            }
        }
        else {
            if (multilevel) {
                c1 <- c(c1, "Number of missing patterns per group:")
                c2 <- c(c2, "")
                c3 <- c(c3, "")
                for (g in 1:datasummary$ngroups) {
                  c1 <- c(c1, paste(sprintf("  %-40s", datasummary$group.label[g]),
                    "-- level 1"))
                  c2 <- c(c2, datasummary$npatterns[g])
                  c3 <- c(c3, "")
                  if (!is.null(datasummary$npatterns2)) {
                    c1 <- c(c1, paste(sprintf("  %-40s", datasummary$group.label[g]),
                      "-- level 2"))
                    c2 <- c(c2, datasummary$npatterns2[g])
                    c3 <- c(c3, "")
                  }
                }
            }
            else {
                c1 <- c(c1, "Number of missing patterns per group:")
                c2 <- c(c2, "")
                c3 <- c(c3, "")
                for (g in 1:datasummary$ngroups) {
                  c1 <- c(c1, sprintf("  %-40s", datasummary$group.label[g]))
                  c2 <- c(c2, datasummary$npatterns[g])
                  c3 <- c(c3, "")
                }
            }
        }
    }
    if (!is.null(datasummary$sampling.weights)) {
        c1 <- c(c1, "Sampling weights variable")
        c2 <- c(c2, datasummary$sampling.weights)
        c3 <- c(c3, "")
    }
    c1 <- format(c1, width = 43L)
    c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L, justify = "right")
    c3 <- format(c3, width = 8L + nd, justify = "right")
    if (threecolumn) {
        M <- cbind(c1, c2, c3, deparse.level = 0)
    }
    else {
        M <- cbind(c1, c2, deparse.level = 0)
    }
    colnames(M) <- rep("", ncol(M))
    rownames(M) <- rep(" ", nrow(M))
	 BSkyFormat(M, singleTableOutputHeader= "Dataset summary")
    #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
    invisible(M)
}