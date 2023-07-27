

print.lavaan.summary_bsky <- function (x, ..., nd = 3L)
{
    y <- unclass(x)
    ND <- attr(y, "nd")
    if (!is.null(ND) && is.numeric(ND)) {
        nd <- as.integer(ND)
    }
    if (!is.null(y$header)) {
        lavaan.version <- y$header$lavaan.version
        sam.approach <- y$header$sam.approach
        optim.method <- y$header$optim.method
        optim.iterations <- y$header$optim.iterations
        optim.converged <- y$header$optim.converged
        if (sam.approach) {
            cat("This is ", sprintf("lavaan %s", lavaan.version),
                " -- using the SAM approach to SEM\n", sep = "")
        }
        else {
            cat(sprintf("lavaan %s ", lavaan.version))
            if (optim.method == "none") {
                cat("-- DRY RUN with 0 iterations --\n")
            }
            else if (optim.iterations > 0) {
                if (optim.converged) {
                  if (optim.iterations == 1L) {
                    cat("ended normally after 1 iteration\n")
                  }
                  else {
                    cat(sprintf("ended normally after %i iterations\n",
                      optim.iterations))
                  }
                }
                else {
                  if (optim.iterations == 1L) {
                    cat("did NOT end normally after 1 iteration\n")
                  }
                  else {
                    cat(sprintf("did NOT end normally after %i iterations\n",
                      optim.iterations))
                  }
                  cat("** WARNING ** Estimates below are most likely unreliable\n")
                }
            }
            else {
                cat("did not run (perhaps do.fit = FALSE)?\n")
                cat("** WARNING ** Estimates below are simply the starting values\n")
            }
        }
    }
    if (!is.null(y$optim)) {
        estimator <- y$optim$estimator
        estimator.args <- y$optim$estimator.args
        optim.method <- y$optim$optim.method
        npar <- y$optim$npar
        eq.constraints <- y$optim$eq.constraints
        nrow.ceq.jac <- y$optim$nrow.ceq.jac
        nrow.cin.jac <- y$optim$nrow.cin.jac
        nrow.con.jac <- y$optim$nrow.con.jac
        con.jac.rank <- y$optim$con.jac.rank
        cat("\n")
        c1 <- c("Estimator")
        tmp.est <- toupper(estimator)
        if (tmp.est == "DLS") {
            dls.first.letter <- substr(estimator.args$dls.GammaNT,
                1L, 1L)
            tmp.est <- paste("DLS-", toupper(dls.first.letter),
                sep = "")
        }
        c2 <- tmp.est
        if (!is.null(estimator.args) && length(estimator.args) >
            0L) {
            if (estimator == "DLS") {
                c1 <- c(c1, "Estimator DLS value for a")
                c2 <- c(c2, estimator.args$dls.a)
            }
        }
        c1 <- c(c1, "Optimization method", "Number of model parameters")
        c2 <- c(c2, toupper(optim.method), npar)
        if (eq.constraints) {
            c1 <- c(c1, "Number of equality constraints")
            c2 <- c(c2, nrow.ceq.jac)
        }
        if (nrow.cin.jac > 0L) {
            c1 <- c(c1, "Number of inequality constraints")
            c2 <- c(c2, nrow.cin.jac)
        }
        if (nrow.con.jac > 0L) {
            if (con.jac.rank == (nrow.ceq.jac + nrow.cin.jac)) {
            }
            else {
                c1 <- c(c1, "Row rank of the constraints matrix")
                c2 <- c(c2, con.jac.rank)
            }
        }
        c1 <- format(c1, width = 40L)
        c2 <- format(c2, width = 11L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        M <- cbind(c1, c2, deparse.level = 0)
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
       # write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
	    BSkyFormat(M, singleTableOutputHeader= "Model Information")
    }
    if (!is.null(y$sam.header)) {
        cat("\n")
        sam.method <- y$sam.header$sam.method
        sam.local.options <- y$sam.header$sam.local.options
        sam.mm.list <- y$sam.header$sam.mm.list
        sam.mm.estimator <- y$sam.header$sam.mm.estimator
        sam.struc.estimator <- y$sam.header$sam.struc.estimator
        c1 <- c("SAM method")
        c2 <- toupper(sam.method)
        if (sam.method == "local") {
            c1 <- c(c1, "Mapping matrix M method")
            c2 <- c(c2, sam.local.options$M.method)
        }
        c1 <- c(c1, "Number of measurement blocks")
        c2 <- c(c2, length(sam.mm.list))
        c1 <- c(c1, "Estimator measurement part")
        c2 <- c(c2, sam.mm.estimator)
        c1 <- c(c1, "Estimator  structural part")
        c2 <- c(c2, sam.struc.estimator)
        c1 <- format(c1, width = 40L)
        c2 <- format(c2, width = 11L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        M <- cbind(c1, c2, deparse.level = 0)
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		 BSkyFormat(M, singleTableOutputHeader= paste(c1, c2,collapse =" "))
    }
    if (!is.null(y$rotation)) {
        cat("\n")
        rotation <- y$rotation
        rotation.args <- y$rotation.args
        c1 <- c2 <- character(0L)
        c1 <- c(c1, "Rotation method")
        if (rotation$rotation == "none") {
            MM <- toupper(rotation$rotation)
        }
        else if (rotation$rotation.args$orthogonal) {
            MM <- paste(toupper(rotation$rotation), " ", "ORTHOGONAL",
                sep = "")
        }
        else {
            MM <- paste(toupper(rotation$rotation), " ", "OBLIQUE",
                sep = "")
        }
        c2 <- c(c2, MM)
        if (rotation$rotation != "none") {
            if (rotation$rotation == "geomin") {
                c1 <- c(c1, "Geomin epsilon")
                c2 <- c(c2, rotation$rotation.args$geomin.epsilon)
            }
            else if (rotation$rotation == "orthomax") {
                c1 <- c(c1, "Orthomax gamma")
                c2 <- c(c2, rotation$rotation.args$orthomax.gamma)
            }
            else if (rotation$rotation == "cf") {
                c1 <- c(c1, "Crawford-Ferguson gamma")
                c2 <- c(c2, rotation$rotation.args$cf.gamma)
            }
            else if (rotation$rotation == "oblimin") {
                c1 <- c(c1, "Oblimin gamma")
                c2 <- c(c2, rotation$rotation.args$oblimin.gamma)
            }
            else if (rotation$rotation == "promax") {
                c1 <- c(c1, "Promax kappa")
                c2 <- c(c2, rotation$rotation.args$promax.kappa)
            }
            c1 <- c(c1, "Rotation algorithm (rstarts)")
            tmp <- paste(toupper(rotation$rotation.args$algorithm),
                " (", rotation$rotation.args$rstarts, ")", sep = "")
            c2 <- c(c2, tmp)
            c1 <- c(c1, "Standardized metric")
            if (rotation$rotation.args$std.ov) {
                c2 <- c(c2, "TRUE")
            }
            else {
                c2 <- c(c2, "FALSE")
            }
            c1 <- c(c1, "Row weights")
            tmp.txt <- rotation$rotation.args$row.weights
            c2 <- c(c2, paste(toupper(substring(tmp.txt, 1, 1)),
                substring(tmp.txt, 2), sep = ""))
        }
        c1 <- format(c1, width = 33L)
        c2 <- format(c2, width = 18L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        M <- cbind(c1, c2, deparse.level = 0)
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		 BSkyFormat(M, singleTableOutputHeader= paste(c1, c2,collapse =" "))
    }
    if (!is.null(y$data)) {
        cat("\n")
        #lavaan:::lav_data_print_short(y$data, nd = nd)
		lav_data_print_short_aar(y$data, nd = nd)
    }	
    if (!is.null(y$sam)) {
        cat("\n")
        sam.method <- y$sam$sam.method
        sam.mm.table <- y$sam$sam.mm.table
        sam.mm.rel <- y$sam$sam.mm.rel
        sam.struc.fit <- y$sam$sam.struc.fit
        ngroups <- y$sam$ngroups
        group.label <- y$sam$group.label
        tmp <- sam.mm.table
        if (sam.method == "global") {
            cat("Summary Information Measurement Part:\n\n")
        }
        else {
            cat("Summary Information Measurement + Structural:\n\n")
        }
        print(tmp, row.names = rep(" ", nrow(tmp)), nd = nd)
        if (sam.method == "local") {
            c1 <- c2 <- character(0L)
            if (ngroups == 1L) {
                cat("\n")
                cat("  Model-based reliability latent variables:\n\n")
                tmp <- data.frame(as.list(sam.mm.rel[[1]]))
                class(tmp) <- c("lavaan.data.frame", "data.frame")
                #print(tmp, row.names = rep(" ", nrow(tmp)), nd = nd)
				#BSkyFormat(tmp, singleTableOutputHeader= "Print 1")
				BSkyFormat(tmp, singleTableOutputHeader= "Model-based reliability latent variables")
            }
            else {
                #cat("\n")
                #cat("  Model-based reliability latent variables (per group):\n")
                for (g in 1:ngroups) {
                 # cat("\n")
                 # cat("  Group ", g, " [", group.label[g], "]:\n\n",
                  #  sep = "")
				  tableDes = paste ("Model-based reliability latent variables (per group):", "Group", group.label[g])
				  
                  tmp <- data.frame(as.list(sam.mm.rel[[g]]))
                  class(tmp) <- c("lavaan.data.frame", "data.frame")
                  #print(tmp, row.names = rep(" ", nrow(tmp)), nd = nd)
				  #BSkyFormat(tmp, singleTableOutputHeader= "Print 2")
				  BSkyFormat(tmp, singleTableOutputHeader= tableDes)
                }
            }
            #cat("\n")
            #cat("  Summary Information Structural part:\n\n")
            tmp <- data.frame(as.list(sam.struc.fit))
            class(tmp) <- c("lavaan.data.frame", "data.frame")
            #print(tmp, row.names = rep(" ", nrow(tmp)), nd = nd)
			#BSkyFormat(tmp, singleTableOutputHeader= "Print 1")
			BSkyFormat(tmp, singleTableOutputHeader= "Summary Information Structural part")
        }
    }
    if (!is.null(y$test)) {
        cat("\n")
        #lavaan:::lav_test_print(y$test, nd = nd)
		lav_test_print_aar(y$test, nd = nd)
    }
    if (!is.null(y$fit)) {
        #lavaan:::print.lavaan.fitMeasures(y$fit, nd = nd, add.h0 = FALSE)
		print.lavaan.fitMeasures_aar(y$fit, nd = nd, add.h0 = FALSE)
    }
    if (!is.null(y$efa)) {
        CT <- attr(y, "cutoff")
        if (!is.null(CT) && is.numeric(CT)) {
            cutoff <- CT
        }
        else {
            cutoff <- 0.3
        }
        DC <- attr(y, "dot.cutoff")
        if (!is.null(DC) && is.numeric(DC)) {
            dot.cutoff <- DC
        }
        else {
            dot.cutoff <- 0.1
        }
        AL <- attr(y, "alpha.level")
        if (!is.null(AL) && is.numeric(AL)) {
            alpha.level <- AL
        }
        else {
            alpha.level <- 0.01
        }
        for (b in seq_len(y$efa$nblocks)) {
            if (length(y$efa$block.label) > 0L) {
                cat(y$efa$block.label[[b]], ":\n\n", sep = "")
            }
            if (!is.null(y$efa$lambda[[b]])) {
                cat("\n")
                if (!is.null(y$efa$lambda.se[[b]]) && alpha.level >
                  0) {
                  cat("Standardized loadings: (* = significant at ",
                    round(alpha.level * 100), "% level)\n\n",
                    sep = "")
                }
                else {
                  cat("Standardized loadings:\n\n")
                }
                LAMBDA <- unclass(y$efa$lambda[[b]])
                THETA <- unname(unclass(y$efa$theta[[b]]))
                #lavaan:::lav_print_loadings(LAMBDA, nd = nd, cutoff = cutoff,
                 # dot.cutoff = dot.cutoff, alpha.level = alpha.level,
                 # resvar = THETA, x.se = y$efa$lambda.se[[b]])
				  
				 lav_print_loadings_aar(LAMBDA, nd = nd, cutoff = cutoff,
                  dot.cutoff = dot.cutoff, alpha.level = alpha.level,
                  resvar = THETA, x.se = y$efa$lambda.se[[b]])
            }
            if (!is.null(y$efa$sumsq.table[[b]])) {
                cat("\n")
                print(y$efa$sumsq.table[[b]], nd = nd)
            }
            if (!y$efa$orthogonal && !is.null(y$efa$psi[[b]]) &&
                ncol(y$efa$psi[[b]]) > 1L) {
                cat("\n")
                if (!is.null(y$efa$psi.se[[b]]) && alpha.level >
                  0) {
                  cat("Factor correlations: (* = significant at ",
                    round(alpha.level * 100), "% level)\n\n",
                    sep = "")
                }
                else {
                  cat("Factor correlations:\n\n")
                }
                #lavaan:::lav_print_psi(y$efa$psi[[b]], nd = nd, alpha.level = alpha.level,
                 # x.se = y$efa$psi.se[[b]])
				  lav_print_psi_bsky(y$efa$psi[[b]], nd = nd, alpha.level = alpha.level,
                  x.se = y$efa$psi.se[[b]])
            }

            if (!is.null(y$efa$fs.determinacy[[b]])) {
                cat("\n")
                cat("Correlation regression factor scores and factors (determinacy):\n\n")
                print(y$efa$fs.determinacy[[b]], nd = nd)
                cat("\n")
                cat("R2 regression factor scores (= squared correlations):\n\n")
                tmp <- y$efa$fs.determinacy[[b]]
                tmp2 <- tmp * tmp
                class(tmp2) <- c("lavaan.vector", "numeric")
                print(tmp2, nd = nd)
            }
            if (!is.null(y$efa$lambda.structure[[b]])) {
                cat("\n")
                cat("Standardized structure (= LAMBDA %*% PSI):\n\n")
                print(y$efa$lambda.structure[[b]], nd = nd)
            }
            if (!is.null(y$efa$theta.se[[b]])) {
                cat("\n")
                cat("Standard errors standardized loadings:\n\n")
                print(y$efa$lambda.se[[b]], nd = nd)
            }
            if (!is.null(y$efa$lambda.zstat[[b]])) {
                cat("\n")
                cat("Z-statistics standardized loadings:\n\n")
                print(y$efa$lambda.zstat[[b]], nd = nd)
            }
            if (!is.null(y$efa$lambda.pvalue[[b]])) {
                cat("\n")
                cat("P-values standardized loadings:\n\n")
                print(y$efa$lambda.pvalue[[b]], nd = nd)
            }
            if (!is.null(y$efa$theta.se[[b]])) {
                cat("\n")
                cat("Standard errors unique variances:\n\n")
                print(y$efa$theta.se[[b]], nd = nd)
            }
            if (!is.null(y$efa$theta.zstat[[b]])) {
                cat("\n")
                cat("Z-statistics unique variances:\n\n")
                print(y$efa$theta.zstat[[b]], nd = nd)
            }
            if (!is.null(y$efa$theta.pvalue[[b]])) {
                cat("\n")
                cat("P-values unique variances:\n\n")
                print(y$efa$theta.pvalue[[b]], nd = nd)
            }
            if (!is.null(y$efa$theta.se[[b]])) {
                cat("\n")
                cat("Standard errors factor correlations:\n\n")
                print(y$efa$psi.se[[b]], nd = nd)
            }
            if (!is.null(y$efa$psi.zstat[[b]])) {
                cat("\n")
                cat("Z-statistics factor correlations:\n\n")
                print(y$efa$psi.zstat[[b]], nd = nd)
            }
            if (!is.null(y$efa$psi.pvalue[[b]])) {
                cat("\n")
                cat("P-values factor correlations:\n\n")
                print(y$efa$psi.pvalue[[b]], nd = nd)
            }
        }
        cat("\n")
    }
    if (!is.null(y$pe) && is.null(y$efa)) {
        PE <- y$pe
        class(PE) <- c("lavaan.parameterEstimates", "lavaan.data.frame",
            "data.frame")
        #print(PE, nd = nd)
		print.lavaan.parameterEstimates_bsky(PE)
		#PE = data.frame(PE)
		# BSkyFormat(PE, singleTableOutputHeader= "AAR Parameter estimates")
    }
    if (!is.null(y$mi)) {
        cat("Modification Indices:\n\n")
        MI <- y$mi
        rownames(MI) <- NULL
        print(MI, nd = nd)
		BSkyFormat(MI, singleTableOutputHeader= "Modification indices")
		
    }
    invisible(y)
}