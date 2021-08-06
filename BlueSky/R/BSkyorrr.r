BSkyorrr <- function (x, conf.level = 0.95, verbose = !quiet, quiet = TRUE,
    digits = 3, relrisk = FALSE)
{
    if (any(dim(x) != c(2, 2))) 
   {
        stop("expecting something 2 x 2")
    }
    x <- as.matrix(x)
    names(x) <- NULL
    row.names(x) <- NULL
    colnames(x) <- NULL
    rowsums <- rowSums(x)
    p1 <- x[1, 1]/rowsums[1]
    p2 <- x[2, 1]/rowsums[2]
    o1 <- p1/(1 - p1)
    o2 <- p2/(1 - p2)
    RR <- p2/p1
    OR <- o2/o1
    crit <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
    names(RR) <- "RR"
    log.RR <- log(RR)
    SE.log.RR <- sqrt(sum(x[, 2]/x[, 1]/rowsums))
    log.lower.RR <- log.RR - crit * SE.log.RR
    log.upper.RR <- log.RR + crit * SE.log.RR
    lower.RR <- exp(log.lower.RR)
    upper.RR <- exp(log.upper.RR)
    names(OR) <- "OR"
    log.OR <- log(OR)
    SE.log.OR <- sqrt(sum(1/x))
    log.lower.OR <- log.OR - crit * SE.log.OR
    log.upper.OR <- log.OR + crit * SE.log.OR
    lower.OR <- exp(log.lower.OR)
    upper.OR <- exp(log.upper.OR)
    res <- if (relrisk) 
    {
        structure(RR, p1 = p1, p2 = p2, o1 = o1, o2 = o2, OR = OR,
            lower.OR = lower.OR, upper.OR = upper.OR, RR = RR,
            lower.RR = lower.RR, upper.RR = upper.RR, conf.level = conf.level,
            class = c("relrisk", "numeric"))
    }
    else 
   {
        structure(OR, p1 = p1, p2 = p2, o1 = o1, o2 = o2, OR = OR,
            lower.OR = lower.OR, upper.OR = upper.OR, RR = RR,
            lower.RR = lower.RR, upper.RR = upper.RR, conf.level = conf.level,
            class = c("oddsRatio", "numeric"))
    }
    if (verbose)
    {
        print(summary(res))
    }
    res
}