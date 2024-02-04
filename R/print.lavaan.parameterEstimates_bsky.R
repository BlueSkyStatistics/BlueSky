
print.lavaan.parameterEstimates_bsky <- function (x, ..., nd = 3L)
{
    num.format <- paste("%", max(8L, nd + 5L), ".", nd, "f",
        sep = "")
    int.format <- paste("%", max(8L, nd + 5L), "d", sep = "")
    char.format <- paste("%", max(8L, nd + 5L), "s", sep = "")
    GSECTIONS <- c("Latent Variables", "Composites", "Regressions",
        "Covariances", "Intercepts", "Thresholds", "Variances",
        "Scales y*", "Group Weight", "R-Square")
    ASECTIONS <- c("Defined Parameters", "Constraints")
    header <- attr(x, "header")
    if (is.null(header)) {
        header <- FALSE
    }
    if (header) {
        #cat("\nParameter Estimates:\n\n")
        if (!is.null(x$se)) {
            c1 <- c2 <- character(0L)
            c1 <- c(c1, "Standard errors")
            if (attr(x, "se") == "robust.huber.white") {
                tmp.txt <- "sandwich"
            }
            else {
                tmp.txt <- attr(x, "se")
            }
            c2 <- c(c2, paste(toupper(substring(tmp.txt, 1, 1)),
                substring(tmp.txt, 2), sep = ""))
            if (attr(x, "se") != "bootstrap") {
                if (attr(x, "se") == "robust.huber.white") {
                  c1 <- c(c1, "Information bread")
                }
                else {
                  c1 <- c(c1, "Information")
                }
                tmp.txt <- attr(x, "information")
                c2 <- c(c2, paste(toupper(substring(tmp.txt,
                  1, 1)), substring(tmp.txt, 2), sep = ""))
                if (attr(x, "information") == "observed") {
                  c1 <- c(c1, "Observed information based on")
                  tmp.txt <- attr(x, "observed.information")
                  c2 <- c(c2, paste(toupper(substring(tmp.txt,
                    1, 1)), substring(tmp.txt, 2), sep = ""))
                }
                if (attr(x, "information") %in% c("expected",
                  "first.order") || attr(x, "observed.information") ==
                  "h1") {
                  if (attr(x, "se") == "robust.huber.white" &&
                    attr(x, "h1.information") != attr(x, "h1.information.meat")) {
                    c1 <- c(c1, "Information bread saturated (h1) model")
                  }
                  else {
                    c1 <- c(c1, "Information saturated (h1) model")
                  }
                  tmp.txt <- attr(x, "h1.information")
                  c2 <- c(c2, paste(toupper(substring(tmp.txt,
                    1, 1)), substring(tmp.txt, 2), sep = ""))
                }
                if (attr(x, "se") == "robust.huber.white" &&
                  attr(x, "information.meat") != "first.order") {
                  c1 <- c(c1, "Information meat")
                  tmp.txt <- attr(x, "information.meat")
                  c2 <- c(c2, paste(toupper(substring(tmp.txt,
                    1, 1)), substring(tmp.txt, 2), sep = ""))
                }
                if (attr(x, "se") == "robust.huber.white" &&
                  attr(x, "h1.information.meat") != attr(x, "h1.information")) {
                  c1 <- c(c1, "Information meat saturated (h1) model")
                  tmp.txt <- attr(x, "h1.information.meat")
                  c2 <- c(c2, paste(toupper(substring(tmp.txt,
                    1, 1)), substring(tmp.txt, 2), sep = ""))
                }
            }
            if (attr(x, "se") == "bootstrap" && !is.null(attr(x,
                "bootstrap"))) {
                c1 <- c(c1, "Number of requested bootstrap draws")
                c2 <- c(c2, attr(x, "bootstrap"))
                c1 <- c(c1, "Number of successful bootstrap draws")
                c2 <- c(c2, attr(x, "bootstrap.successful"))
            }
            c1 <- format(c1, width = 38L)
            c2 <- format(c2, width = 13L + max(0, (nd - 3L)) *
                4L, justify = "right")
            M <- cbind(c1, c2, deparse.level = 0)
            colnames(M) <- rep("", ncol(M))
            rownames(M) <- rep(" ", nrow(M))
			
            #write.table(M, row.names = TRUE, col.names = FALSE,
             #   quote = FALSE)
			 BSkyFormat(M, singleTableOutputHeader= "Parameter Estimates")
        }
    }
    if (is.null(x$group)) {
        ngroups <- 1L
        x$group <- rep(1L, length(x$lhs))
    }
    else {
        ngroups <- lavaan:::lav_partable_ngroups(x)
    }
    if (is.null(x$level)) {
        nlevels <- 1L
        x$level <- rep(1L, length(x$lhs))
    }
    else {
        nlevels <- lavaan:::lav_partable_nlevels(x)
    }
    if (is.null(x$block)) {
        x$block <- rep(1L, length(x$lhs))
    }
    y <- as.data.frame(lapply(x, function(x) {
        if (is.integer(x)) {
            sprintf(int.format, x)
        }
        else if (is.numeric(x)) {
            sprintf(num.format, x)
        }
        else {
            x
        }
    }), stringsAsFactors = FALSE)
    y$op <- y$group <- y$rhs <- y$label <- y$exo <- NULL
    y$block <- y$level <- NULL
    y$efa <- NULL
    if (!is.null(y$std.all)) {
        y$std.nox <- NULL
    }
    m <- as.matrix(format.data.frame(y, na.encode = FALSE, justify = "right"))
    rownames(m) <- rep("", nrow(m))
    if (!is.null(x$se)) {
        se.idx <- which(x$se == 0)
        if (length(se.idx) > 0L) {
            m[se.idx, "se"] <- ""
            if (!is.null(x$z)) {
                m[se.idx, "z"] <- ""
            }
            if (!is.null(x$pvalue)) {
                m[se.idx, "pvalue"] <- ""
            }
            if (!is.null(x$t)) {
                m[se.idx, "t"] <- ""
            }
            if (!is.null(x$df)) {
                m[se.idx, "df"] <- ""
            }
        }
        se.idx <- which(is.na(x$se))
        if (length(se.idx) > 0L) {
            if (!is.null(x$z)) {
                m[se.idx, "z"] <- ""
            }
            if (!is.null(x$pvalue)) {
                m[se.idx, "pvalue"] <- ""
            }
            if (!is.null(x$t)) {
                m[se.idx, "t"] <- ""
            }
            if (!is.null(x$df)) {
                m[se.idx, "df"] <- ""
            }
        }
    }
    if (!is.null(x$lower)) {
        b.idx <- which(abs(x$lower - x$est) < sqrt(.Machine$double.eps) &
            (is.na(x$se) | (is.finite(x$se) & x$se != 0)))
        if (length(b.idx) > 0L && !is.null(x$pvalue)) {
            m[b.idx, "pvalue"] <- ""
            if (is.null(x$label)) {
                x$label <- rep("", length(x$lhs))
            }
            x$label[b.idx] <- ifelse(nchar(x$label[b.idx]) >
                0L, paste(x$label[b.idx], "+lb", sep = ""), "lb")
        }
        m <- m[, colnames(m) != "lower"]
    }
    if (!is.null(x$upper)) {
        b.idx <- which(abs(x$upper - x$est) < sqrt(.Machine$double.eps) &
            is.finite(x$se) & x$se != 0)
        if (length(b.idx) > 0L && !is.null(x$pvalue)) {
            m[b.idx, "pvalue"] <- ""
            if (is.null(x$label)) {
                x$label <- rep("", length(x$lhs))
            }
            x$label[b.idx] <- ifelse(nchar(x$label[b.idx]) >
                0L, paste(x$label[b.idx], "+ub", sep = ""), "ub")
        }
        m <- m[, colnames(m) != "upper"]
    }
    if (!is.null(x$fmi)) {
        se.idx <- which(x$se == 0)
        if (length(se.idx) > 0L) {
            m[se.idx, "fmi"] <- ""
            if (!is.null(x$riv))
                m[se.idx, "riv"] <- ""
        }
        not.idx <- which(x$op %in% c(":=", "<", ">", "=="))
        if (length(not.idx) > 0L) {
            if (!is.null(x$fmi)) {
                m[not.idx, "fmi"] <- ""
                if (!is.null(x$riv))
                  m[not.idx, "riv"] <- ""
            }
        }
    }
    if (!is.null(x$Post.SD)) {
        se.idx <- which(x$Post.SD == 0)
        if (length(se.idx) > 0L) {
            m[se.idx, "Post.SD"] <- ""
            if (!is.null(x$psrf)) {
                m[se.idx, "psrf"] <- ""
            }
            if (!is.null(x$PSRF)) {
                m[se.idx, "PSRF"] <- ""
            }
        }
        not.idx <- which(x$op %in% c(":=", "<", ">", "=="))
        if (length(not.idx) > 0L) {
            if (!is.null(x$psrf)) {
                m[not.idx, "psrf"] <- ""
            }
            if (!is.null(x$PSRF)) {
                m[not.idx, "PSRF"] <- ""
            }
        }
    }
    colnames(m)[colnames(m) == "lhs"] <- ""
    colnames(m)[colnames(m) == "op"] <- ""
    colnames(m)[colnames(m) == "rhs"] <- ""
    colnames(m)[colnames(m) == "step"] <- "Step"
    colnames(m)[colnames(m) == "est"] <- "Estimate"
    colnames(m)[colnames(m) == "se"] <- "Std.Err"
    colnames(m)[colnames(m) == "z"] <- "z-value"
    colnames(m)[colnames(m) == "pvalue"] <- "P(>|z|)"
    colnames(m)[colnames(m) == "std.lv"] <- "Std.lv"
    colnames(m)[colnames(m) == "std.all"] <- "Std.all"
    colnames(m)[colnames(m) == "std.nox"] <- "Std.nox"
    colnames(m)[colnames(m) == "prior"] <- "Prior"
    colnames(m)[colnames(m) == "fmi"] <- "FMI"
    if ("t" %in% colnames(m)) {
        colnames(m)[colnames(m) == "t"] <- "t-value"
        colnames(m)[colnames(m) == "P(>|z|)"] <- "P(>|t|)"
        colnames(m)[colnames(m) == "riv"] <- "RIV"
    }
    colnames(m) <- sprintf(char.format, colnames(m))
    if (!is.null(x$Post.Mean)) {
        tmp <- gsub("[ \t]+", "", colnames(m), perl = TRUE)
        col.idx <- which(tmp == "Post.Mean")
        if (length(col.idx) > 0L) {
            tmp.format <- paste("%", max(9, nd + 5), "s", sep = "")
            colnames(m)[col.idx] <- sprintf(tmp.format, colnames(m)[col.idx])
            m[, col.idx] <- sprintf(tmp.format, m[, col.idx])
        }
        col.idx <- which(tmp == "Prior")
        if (length(col.idx) > 0L) {
            MAX <- max(nchar(m[, col.idx])) + 1L
            tmp.format <- paste("%", max(MAX, nd + 5), "s", sep = "")
            colnames(m)[col.idx] <- sprintf(tmp.format, colnames(m)[col.idx])
            m[, col.idx] <- sprintf(tmp.format, m[, col.idx])
        }
    }
    b <- 0L
    for (g in 1:ngroups) {
        if (ngroups > 1L) {
            group.label <- attr(x, "group.label")
            #cat("\n\n")
            #cat("Group ", g, " [", group.label[g], "]:\n", sep = "")
        }
        for (l in 1:nlevels) {
            b <- b + 1L
            ov.names <- lavNames(x, "ov", block = b)
            lv.names <- lavNames(x, "lv", block = b)
            if (nlevels > 1L) {
                level.label <- attr(x, "level.label")
                cat("\n\n")
                cat("Level ", l, " [", level.label[l], "]:\n",
                  sep = "")
            }
            for (s in GSECTIONS) {
                if (s == "Latent Variables") {
                  row.idx <- which(x$op == "=~" & !x$lhs %in%
                    ov.names & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else if (s == "Composites") {
                  row.idx <- which(x$op == "<~" & x$block ==
                    b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else if (s == "Regressions") {
                  row.idx <- which(x$op == "~" & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else if (s == "Covariances") {
                  row.idx <- which(x$op == "~~" & x$lhs != x$rhs &
                    !x$exo & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  y.names <- unique(c(lavNames(x, "eqs.y"), lavNames(x,
                    "ov.ind"), lavNames(x, "lv.ind")))
                  PREFIX <- rep("", length(row.idx))
                  PREFIX[x$rhs[row.idx] %in% y.names] <- "  ."
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx], PREFIX = PREFIX)
                }
                else if (s == "Intercepts") {
                  row.idx <- which(x$op == "~1" & !x$exo & x$block ==
                    b)
                  if (length(row.idx) == 0L)
                    next
                  y.names <- unique(c(lavNames(x, "eqs.y"), lavNames(x,
                    "ov.ind"), lavNames(x, "lv.ind")))
                  PREFIX <- rep("", length(row.idx))
                  PREFIX[x$lhs[row.idx] %in% y.names] <- "  ."
                  m[row.idx, 1] <- lavaan:::.makeNames(x$lhs[row.idx],
                    x$label[row.idx], PREFIX = PREFIX)
                }
                else if (s == "Thresholds") {
                  row.idx <- which(x$op == "|" & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(paste(x$lhs[row.idx],
                    "|", x$rhs[row.idx], sep = ""), x$label[row.idx])
                }
                else if (s == "Variances") {
                  row.idx <- which(x$op == "~~" & x$lhs == x$rhs &
                    !x$exo & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  y.names <- unique(c(lavNames(x, "eqs.y"), lavNames(x,
                    "ov.ind"), lavNames(x, "lv.ind")))
                  PREFIX <- rep("", length(row.idx))
                  PREFIX[x$rhs[row.idx] %in% y.names] <- "  ."
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx], PREFIX = PREFIX)
                }
                else if (s == "Scales y*") {
                  row.idx <- which(x$op == "~*~" & x$block ==
                    b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else if (s == "Group Weight") {
                  row.idx <- which(x$lhs == "group" & x$op ==
                    "%" & x$block == b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else if (s == "R-Square") {
                  row.idx <- which(x$op == "r2" & x$block ==
                    b)
                  if (length(row.idx) == 0L)
                    next
                  m[row.idx, 1] <- lavaan:::.makeNames(x$rhs[row.idx],
                    x$label[row.idx])
                }
                else {
                  row.idx <- integer(0L)
                }
                if (s %in% c("Latent Variables", "Composites",
                  "Regressions", "Covariances")) {
                  nel <- length(row.idx)
                  M <- matrix("", nrow = nel * 2, ncol = ncol(m))
                  colnames(M) <- colnames(m)
                  rownames(M) <- rep("", NROW(M))
                  if (is.null(x$efa)) {
                    LHS <- paste(x$lhs[row.idx], x$op[row.idx])
                  }
                  else {
                    LHS <- paste(x$lhs[row.idx], x$op[row.idx],
                      x$efa[row.idx])
                  }
                  lhs.idx <- seq(1, nel * 2L, 2L)
                  rhs.idx <- seq(1, nel * 2L, 2L) + 1L
                  if (s == "Covariances") {
                    y.names <- unique(c(lavNames(x, "eqs.y"),
                      lavNames(x, "ov.ind"), lavNames(x, "lv.ind")))
                    PREFIX <- rep("", length(row.idx))
                    PREFIX[x$lhs[row.idx] %in% y.names] <- "."
                  }
                  else {
                    PREFIX <- rep("", length(LHS))
                  }
                  M[lhs.idx, 1] <- sprintf("%1s%-15s", PREFIX,
                    LHS)
                  M[rhs.idx, ] <- m[row.idx, ]
                  if (nel > 1L) {
                    del.idx <- integer(0)
                    old.lhs <- ""
                    for (i in 1:nel) {
                      if (LHS[i] == old.lhs) {
                        del.idx <- c(del.idx, lhs.idx[i])
                      }
                      old.lhs <- LHS[i]
                    }
                    if (length(del.idx) > 0L) {
                      M <- M[-del.idx, , drop = FALSE]
                    }
                  }
                  #cat("\n", s, ":\n", sep = "")
				  #print(M, quote = FALSE)
				  #cat("Group ", g, " [", group.label[g], "]:\n", sep = "")
				   if (ngroups > 1L) {
				  s = paste (s, "-", "Group with label: " ,group.label[g])
				  }
				  BSkyFormat(M, singleTableOutputHeader= paste ("Model Parameter Estimates:",s))
                }
                else if (s == "R-Square") {
                  M <- m[row.idx, 1:2, drop = FALSE]
                  colnames(M) <- colnames(m)[1:2]
                  rownames(M) <- rep("", NROW(M))
                  #cat("\n", s, ":\n", sep = "")
                  #print(M, quote = FALSE)
				  if (ngroups > 1L) {
				  s = paste (s, "-", "Group with label: " ,group.label[g])
				  }
				  BSkyFormat(M, singleTableOutputHeader= s)
                }
                else {
                  M <- m[row.idx, , drop = FALSE]
                  colnames(M) <- colnames(m)
                  rownames(M) <- rep("", NROW(M))
                  #cat("\n", s, ":\n", sep = "")
                  #print(M, quote = FALSE)
				  if (ngroups > 1L) {
				  s = paste (s, "-", "Group with label: " ,group.label[g])
				  }
				  BSkyFormat(M, singleTableOutputHeader= paste ("Model Parameter Estimates:",s))
                }
            }
        }
    }
    for (s in ASECTIONS) {
        if (s == "Defined Parameters") {
            row.idx <- which(x$op == ":=")
            m[row.idx, 1] <- lavaan:::.makeNames(x$lhs[row.idx], "")
            M <- m[row.idx, , drop = FALSE]
            colnames(M) <- colnames(m)
        }
        else if (s == "Constraints") {
            row.idx <- which(x$op %in% c("==", "<", ">"))
            if (length(row.idx) == 0)
                next
            m[row.idx, 1] <- .makeConNames(x$lhs[row.idx], x$op[row.idx],
                x$rhs[row.idx], nd = nd)
            m[row.idx, 2] <- sprintf(num.format, abs(x$est[row.idx]))
            M <- m[row.idx, 1:2, drop = FALSE]
            colnames(M) <- c("", sprintf(char.format, "|Slack|"))
        }
        else {
            row.idx <- integer(0L)
        }
        if (length(row.idx) == 0L) {
            next
        }
        rownames(M) <- rep("", NROW(M))
        cat("\n", s, ":\n", sep = "")
		BSkyFormat(M, singleTableOutputHeader= s)
        #print(M, quote = FALSE)
    }
    cat("\n")
    invisible(m)
}