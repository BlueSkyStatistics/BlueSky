process.capability.enhanced <- function (object, spec.limits, target, std.dev, nsigmas, confidence.level = 0.95, 
    breaks = "scott", add.stats = TRUE, print = TRUE, capability.type = "potential", digits = getOption("digits"), 
    restore.par = TRUE) 
{
    if ((missing(object)) | (!inherits(object, "qcc"))) 
        stop("an object of class 'qcc' is required")
    if (!(object$type == "xbar" | object$type == "xbar.one")) 
        stop("Process Capability Analysis only available for charts type \"xbar\" and \"xbar.one\" charts")
    x <- as.vector(object$data)
    x <- x[!is.na(x)]
    sizes <- object$sizes
    center <- object$center
    if (missing(std.dev)) 
        std.dev <- object$std.dev
    n <- length(x)
	
	# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	if(capability.type == "overall")
	{
		#title <- paste("Overall Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		title <- paste("Overall Process Capability Analysis\nfor",
			object$data.name)
	}
	else
	{
		#title <- paste("Potential (Within) Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		title <- paste("Potential (Within) Process Capability Analysis\nfor",
			object$data.name)
	}
	
    if (missing(spec.limits)) 
        stop("specification limits must be provided")
    spec.limits <- as.vector(spec.limits)[1:2]
    LSL <- spec.limits[1]
    if (!(is.numeric(LSL) & is.finite(LSL))) 
        LSL <- NA
    USL <- spec.limits[2]
    if (!(is.numeric(USL) & is.finite(USL))) 
        USL <- NA
    if (is.na(LSL) & is.na(USL)) 
        stop("invalid specification limits")
    has.target <- (!missing(target))
    if (!has.target) {
        target <- mean(spec.limits, na.rm = TRUE)
        if (!is.na(LSL) & !is.na(USL)) 
            has.target <- TRUE
    }
    if (is.na(LSL)) {
        if (target > USL) 
            warning("target value larger than one-sided specification limit...")
    }
    if (is.na(USL)) {
        if (target < LSL) 
            warning("target value smaller than one-sided specification limit...")
    }
    if (!is.na(LSL) & !is.na(USL)) {
        if (target < LSL || target > USL) 
            warning("target value is not within specification limits...")
    }
    if (missing(nsigmas)) 
        if (is.null(object$nsigmas)) 
            stop("nsigmas not available in the 'qcc' object. Please provide nsigmas.")
        else nsigmas <- object$nsigmas
    if (confidence.level < 0 | confidence.level > 1) 
        stop("the argument confidence.level must be a value between 0 and 1")
    Cp <- (USL - LSL)/(2 * nsigmas * std.dev)
    Cp.u <- (USL - center)/(nsigmas * std.dev)
    Cp.l <- (center - LSL)/(nsigmas * std.dev)
    Cp.k <- min(Cp.u, Cp.l)
    Cpm <- Cp/sqrt(1 + ((center - target)/std.dev)^2)
    alpha <- 1 - confidence.level
    Cp.limits <- Cp * sqrt(qchisq(c(alpha/2, 1 - alpha/2), n - 
        1)/(n - 1))
    Cp.u.limits <- Cp.u * (1 + c(-1, 1) * qnorm(confidence.level) * 
        sqrt(1/(9 * n * Cp.u^2) + 1/(2 * (n - 1))))
    Cp.l.limits <- Cp.l * (1 + c(-1, 1) * qnorm(confidence.level) * 
        sqrt(1/(9 * n * Cp.l^2) + 1/(2 * (n - 1))))
    Cp.k.limits <- Cp.k * (1 + c(-1, 1) * qnorm(1 - alpha/2) * 
        sqrt(1/(9 * n * Cp.k^2) + 1/(2 * (n - 1))))
    df <- n * (1 + ((center - target)/std.dev)^2)/(1 + 2 * ((center - 
        target)/std.dev)^2)
    Cpm.limits <- Cpm * sqrt(qchisq(c(alpha/2, 1 - alpha/2), 
        df)/df)
    names(Cp.limits) <- names(Cp.k.limits) <- names(Cpm.limits) <- c(paste(round(100 * 
        alpha/2, 1), "%", sep = ""), paste(round(100 * 
        (1 - alpha/2), 1), "%", sep = ""))
    if (is.na(LSL)) 
        exp.LSL <- NA
    else {
        exp.LSL <- pnorm((LSL - center)/std.dev) * 100
        if (exp.LSL < 0.01) 
            exp.LSL <- 0
    }
    if (is.na(USL)) 
        exp.USL <- NA
    else {
        exp.USL <- (1 - pnorm((USL - center)/std.dev)) * 100
        if (exp.USL < 0.01) 
            exp.USL <- 0
    }
    obs.LSL <- sum(x < LSL)/n * 100
    obs.USL <- sum(x > USL)/n * 100
    xlim <- range(x, USL, LSL, target, na.rm = TRUE)
    xlim <- xlim + diff(xlim) * c(-0.1, 0.1)
    xx <- seq(min(xlim), max(xlim), length = 250)
    dx <- dnorm(xx, center, std.dev)
    h <- hist(x, breaks = breaks, plot = FALSE)
    ylim <- range(h$density, dx)
    ylim <- ylim + diff(ylim) * c(0, 0.05)
    tab <- cbind(c(Cp, Cp.l, Cp.u, Cp.k, Cpm), rbind(Cp.limits, 
        Cp.l.limits, Cp.u.limits, Cp.k.limits, Cpm.limits))
	
	# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	if(capability.type == "overall")
	{
		rownames(tab) <- c("Pp", "Pp_l", "Pp_u", 
			"Pp_k", "Ppm")
	}
	else
	{
		rownames(tab) <- c("Cp", "Cp_l", "Cp_u", 
			"Cp_k", "Cpm")
	}
	
    colnames(tab) <- c("Value", names(Cp.limits))
    oldpar <- par(no.readonly = TRUE)
    if (restore.par) 
        on.exit(par(oldpar))
    mar <- c(4.1, 2.1, 3.6, 2.1)
    par(bg = qcc.options("bg.margin"), cex = oldpar$cex * 
        qcc.options("cex"), mar = if (add.stats) 
        pmax(mar, c(8.6 + is.null(center) * -1, 0, 0, 0))
    else mar)
    plot(0, 0, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, 
        ylab = "", xlab = "")
    usr <- par()$usr
    rect(usr[1], usr[3], usr[2], usr[4], col = qcc.options("bg.figure"))
    axis(1)
    box()
    top.line <- par("mar")[3] - length(capture.output(cat(title))) - 
        0.5
    mtext(title, side = 3, line = top.line, font = par("font.main"), 
        cex = qcc.options("cex"), col = par("col.main"))
    plot(h, add = TRUE, freq = FALSE)
    abline(v = c(LSL, USL), col = 2, lty = 3, lwd = 2)
    text(LSL, usr[4], "LSL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    text(USL, usr[4], "USL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    if (has.target) {
        abline(v = target, col = 2, lty = 2, lwd = 2)
        text(target, usr[4], "Target", pos = 3, offset = 0.2, 
            cex = 0.8, xpd = TRUE)
    }
    lines(xx, dx, lty = 2)
    if (add.stats) {
        plt <- par()$plt
        px <- diff(usr[1:2])/diff(plt[1:2])
        xfig <- c(usr[1] - px * plt[1], usr[2] + px * (1 - plt[2]))
        at.col <- xfig[1] + diff(xfig[1:2]) * c(0.07, 0.35, 0.56, 
            0.75)
        top.line <- 3
        mtext(paste("Number of obs = ", n, sep = ""), 
            side = 1, line = top.line, adj = 0, at = at.col[1], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
        mtext(paste("Center = ", signif(center, digits), 
            sep = ""), side = 1, line = top.line + 1, adj = 0, 
            at = at.col[1], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("StdDev = ", signif(std.dev, digits), 
            sep = ""), side = 1, line = top.line + 2, adj = 0, 
            at = at.col[1], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(ifelse(has.target, paste("Target = ", signif(target, 
            digits), sep = ""), paste("Target = ")), 
            side = 1, line = top.line, adj = 0, at = at.col[2], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
        mtext(paste("LSL = ", ifelse(is.na(LSL), "", 
            signif(LSL, digits)), sep = ""), side = 1, 
            line = top.line + 1, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("USL = ", ifelse(is.na(USL), "", 
            signif(USL, digits)), sep = ""), side = 1, 
            line = top.line + 2, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
			
		# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
		if(capability.type == "overall")
		{
			mtext(paste("Pp     = ", ifelse(is.na(Cp), "", 
				signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_l  = ", ifelse(is.na(Cp.l), "", 
				signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_u = ", ifelse(is.na(Cp.u), "", 
				signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_k = ", ifelse(is.na(Cp.k), "", 
				signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Ppm  = ", ifelse(is.na(Cpm), "", 
				signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
		}
		else
		{
						mtext(paste("Cp     = ", ifelse(is.na(Cp), "", 
				signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_l  = ", ifelse(is.na(Cp.l), "", 
				signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_u = ", ifelse(is.na(Cp.u), "", 
				signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_k = ", ifelse(is.na(Cp.k), "", 
				signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cpm  = ", ifelse(is.na(Cpm), "", 
				signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
		}
		
        mtext(paste("Exp<LSL ", ifelse(is.na(exp.LSL), 
            "", paste(signif(exp.LSL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Exp>USL ", ifelse(is.na(exp.USL), 
            "", paste(signif(exp.USL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 1, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs<LSL ", ifelse(is.na(obs.LSL), 
            "", paste(signif(obs.LSL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 2, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs>USL ", ifelse(is.na(obs.USL), 
            "", paste(signif(obs.USL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 3, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
    }
    if (print) {
        cat("\nProcess Capability Analysis\n")
        cat("\nCall:\n", deparse(match.call()), "\n\n", 
            sep = "")
        cat(paste(formatC("Number of obs = ", width = 16), 
            formatC(n, width = 12, flag = "-"), formatC("Target = ", 
                width = 10), ifelse(has.target, formatC(signif(target, 
                digits = digits), flag = "-"), ""), 
            "\n", sep = ""))
        cat(paste(formatC("Center = ", width = 16), formatC(signif(center, 
            digits = digits), width = 12, flag = "-"), 
            formatC("LSL = ", width = 10), ifelse(is.na(LSL), 
                "", formatC(signif(LSL, digits = digits), 
                  flag = "-")), "\n", sep = ""))
        cat(paste(formatC("StdDev = ", width = 16), formatC(signif(std.dev, 
            digits = digits), width = 12, flag = "-"), 
            formatC("USL = ", width = 10), ifelse(is.na(USL), 
                "", formatC(signif(USL, digits = digits), 
                  flag = "-")), "\n", sep = ""))
        cat("\nCapability indices:\n\n")
        print(tab, digits = 4, na.print = "", print.gap = 2)
        cat("\n")
        cat(paste("Exp<LSL", ifelse(is.na(exp.LSL), "\t", 
            paste(format(exp.LSL, digits = 2), "%\t", sep = "")), 
            "Obs<LSL", ifelse(is.na(obs.LSL), "", 
                paste(format(obs.LSL, digits = 2), "%\n", 
                  sep = ""))))
        cat(paste("Exp>USL", ifelse(is.na(exp.USL), "\t", 
            paste(format(exp.USL, digits = 2), "%\t", sep = "")), 
            "Obs>USL", ifelse(is.na(obs.USL), "", 
                paste(format(obs.USL, digits = 2), "%\n", 
                  sep = ""))))
    }
    invisible(list(nobs = n, center = center, std.dev = std.dev, 
        target = target, spec.limits = {
            sl <- c(LSL, USL)
            names(sl) <- c("LSL", "USL")
            sl
        }, indices = tab, exp = {
            exp <- c(exp.LSL, exp.USL)/100
            names(exp) <- c("Exp < LSL", "Exp > USL")
            exp
        }, obs = {
            obs <- c(obs.LSL, obs.USL)/100
            names(obs) <- c("Obs < LSL", "Obs > USL")
            obs
        }))
}


test.special.causes <- function(object, test1 = TRUE, one.point.k.stdv = 3, test2 = TRUE, k.run.same.side = 9, 
                                        test3 = FALSE, k.run.increase.decrease = 6, test4 = FALSE, k.run.alternating = 14,
										test5 = FALSE, k.plusone.run.beyond.2dev = 2, test6 = FALSE, k.plusone.run.beyond.1dev = 4, 
										test7 = FALSE, k.run.within.1dev = 15, test8 = FALSE, k.run.beyond.1dev = 8, either.side = TRUE, 
										print = TRUE, digits = 4, debug = FALSE)
{
		# if ((missing(object)) | (!inherits(object, "qcc"))) 
			# stop("an object of class `qcc' is required")
			
		
		type <- object$type
		sizes <- object$sizes
		std.dev <- object$std.dev
		center <- object$center
		stats <- object$statistics
		newstats <- object$newstats
		statistics <- c(stats, newstats)
		
		violators = violating.runs.indices(object, test1=test1, test2= test2, test3=test3, test4= test4,
												   test5=test5, test6= test6, test7=test7, test8= test8,
												   beyond.kdev.one.point = one.point.k.stdv, run.length = k.run.same.side, 
												   increase.decrease.run.length = k.run.increase.decrease,
		                                           alternating.run.length = k.run.alternating, 
												   beyond.plusone.2dev.run.length = k.plusone.run.beyond.2dev, 
												   beyond.plusone.1dev.run.length = k.plusone.run.beyond.1dev,
												   within.1dev.run.length = k.run.within.1dev, beyond.1dev.run.length = k.run.beyond.1dev,
												   either.side = either.side)
		
		selcted_tests_choices = c("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8")
		selcted_tests = selcted_tests_choices[c(test1, test2, test3, test4, test5, test6, test7, test8)]
		selected_test_str = paste(selcted_tests, collapse = ', ')
		
		if(print == TRUE)
		{
			if(length(selcted_tests) > 0)
			{
				BSkyFormat(paste("\nDetails for the selected tests (", selected_test_str ,") performed for special causes in control charts"))
			}
			
			###################
			
			if(test1)
			{
				BSkyFormat(paste("Test 1: One point more than", one.point.k.stdv, "σ from center line"))
				if(length(violators$beyond.kdev.one.point.index) == 1)
				{
				  cat("\nonly sample", dimnames(violators$beyond.kdev.one.point.index)[[2]], ">",one.point.k.stdv,"standard deviation from the center line\n")
				  BSkyFormat(violators$beyond.kdev.one.point.index, decimalDigitsRounding = digits)
				}
				else if(length(violators$beyond.kdev.one.point.index) == 0)
				{
					cat("\nno sample found >",one.point.k.stdv,"standard deviation from the center line\n")
				}
				else
				{
					cat("\ninfo only: more than one sample found (", dimnames(violators$beyond.kdev.one.point.index)[[2]], ") >",one.point.k.stdv,"standard deviation from the center line\n")
					BSkyFormat(violators$beyond.kdev.one.point.index, decimalDigitsRounding = digits)
				}
			}
			
			##################
			
			if(test2)
			{
				BSkyFormat(paste("Test 2:", k.run.same.side, "points in a row on the same side of the center line"))
				if(length(violators$run.above.indices) > 0)
				{
					cat("\n",k.run.same.side, "points in a row above the center line - violating samples (", dimnames(violators$run.above.indices)[[2]], ")\n")
					BSkyFormat(violators$run.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$run.below.indices) > 0)
				{
					cat("\n",k.run.same.side, "points in a row below the center line - violating samples (", dimnames(violators$run.below.indices)[[2]], ")\n")
					BSkyFormat(violators$run.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$run.below.indices) == 0 && length(violators$run.below.indices) == 0)
				{
					cat("\n",k.run.same.side, "points in a row above or below the center line - no violating sample found\n")
				}
			}
			 
			
			###############
			
			if(test3)
			{
				BSkyFormat(paste("Test 3:", k.run.increase.decrease, "points in a row, all increasing or all decreasing"))
				if(length(violators$increase.decrease.run.indices) > 0)
				{
					cat("\n",k.run.increase.decrease, "points in a row, increasing or decreasing - violating samples (", dimnames(violators$increase.decrease.run.indices)[[2]], ")\n")
					BSkyFormat(violators$increase.decrease.run.indices, decimalDigitsRounding = digits)
				}
				else
				{
					cat("\n",k.run.increase.decrease, "points in a row, increasing or decreasing - no violating sample found\n")
				}
			}
			
			############
			
			if(test4)
			{
				BSkyFormat(paste("Test 4:", k.run.alternating, "points in a row, alternating up and down"))
				if(length(violators$alternate.run.indices) > 0)
				{
					cat("\n",k.run.alternating, "points in a row, alternating up and down - violating samples (", dimnames(violators$alternate.run.indices)[[2]], ")\n")
					BSkyFormat(violators$alternate.run.indices, decimalDigitsRounding = digits) #, outputTableRenames = c(rep("",length(violators$alternate.run.indices))))
				}
				else
				{
					cat("\n",k.run.alternating, "points in a row, alternating up and down - no violating sample found\n")
				}
			}
			
			##################
			
			if(test5)
			{
				BSkyFormat(paste("Test 5:", k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points more than 2σ from the center line (same side)"))
				if(length(violators$beyond.plusone.2dev.above.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.plusone.2dev.above.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.2dev.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.2dev.below.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation below the center line  - violating samples (", dimnames(violators$beyond.plusone.2dev.below.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.2dev.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.2dev.above.indices) == 0 && length(violators$beyond.plusone.2dev.below.indices) == 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation above or below the center line  - no violating sample found\n")
				}
			}
			
			##################
			
			if(test6)
			{
				BSkyFormat(paste("Test 6:", k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points more than 1σ from the center line (same side)"))
				if(length(violators$beyond.plusone.1dev.above.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points > 1 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.plusone.1dev.above.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.1dev.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.2dev.below.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points > 1 standard deviation below the center line  - violating samples (", dimnames(violators$beyond.plusone.1dev.below.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.1dev.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.1dev.above.indices) == 0 && length(violators$beyond.plusone.1dev.below.indices) == 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 1 standard deviation above or below the center line  - no violating sample found\n")
				}
			}
			
			################## 
			
			if(test7)
			{
				if(either.side == TRUE)
				{
					BSkyFormat(paste("Test 7:", k.run.within.1dev, "points in a row within 1σ of center line (either side)"))
					if(length(violators$within.1dev.above.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation on either side of the center line  - violating samples (", dimnames(violators$within.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					# if(length(violators$within.1dev.below.indices) > 0)
					# {
						# cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation below the center line  - violating samples (", dimnames(violators$within.1dev.below.indices)[[2]], ")\n")
						# BSkyFormat(violators$within.1dev.below.indices)
					# }
					
					if(length(violators$within.1dev.above.indices) == 0 && length(violators$within.1dev.below.indices) == 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation above or below the center line  - no violating sample found\n")
					}
				}
				else
				{
					BSkyFormat(paste("Test 7:", k.run.within.1dev, "points in a row within 1σ of center line (on the same side)"))
					if(length(violators$within.1dev.above.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation above the center line  - violating samples (", dimnames(violators$within.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$within.1dev.below.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation below the center line  - violating samples (", dimnames(violators$within.1dev.below.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.below.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$within.1dev.above.indices) == 0 && length(violators$within.1dev.below.indices) == 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation on either side of the center line  - no violating sample found\n")
					}
				}
			}
				
			###################
			
			if(test8)
			{
				if(either.side == TRUE)
				{
					BSkyFormat(paste("Test 8:", k.run.beyond.1dev, "points in a row more than 1σ from center line (either side)"))
					if(length(violators$beyond.1dev.above.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation on either side of the center line  - violating samples (", dimnames(violators$beyond.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					# if(length(violators$beyond.1dev.below.indices) > 0)
					# {
						# cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation below the center line  - violating samples (", BSkyFormat(violators$beyond.1dev.below.indices)[[2]], ")\n")
						# BSkyFormat(violators$beyond.1dev.below.indices)
					# }
					
					if(length(violators$beyond.1dev.above.indices) == 0 && length(violators$beyond.1dev.below.indices) == 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation on either side of the center line  - no violating sample found\n")
					}
				}
				else
				{
					BSkyFormat(paste("Test 8:", k.run.beyond.1dev, "points in a row more than 1σ from center line (on the same side)"))
					if(length(violators$beyond.1dev.above.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$beyond.1dev.below.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation below the center line  - violating samples (", BSkyFormat(violators$beyond.1dev.below.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.below.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$beyond.1dev.above.indices) == 0 && length(violators$beyond.1dev.below.indices) == 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation above or below the center line  - no violating sample found\n")
					}
				}
			}
		}
	
	
		##############################################################################################
		# preparing the indices for plot.qcc() to color orange (violating.runs) or red (beyond limits)
		##############################################################################################
		
		statistics <- c(object$statistics, object$newstats)
		beyond_limits <- as.numeric(names(statistics)[sort(beyond.limits(object, limits = object$limits))])
		# BSkyFormat(beyond_limits)
		# BSkyFormat(which(names(statistics) %in%beyond_limits))
		
		bl <- sort(beyond.limits(object, limits = object$limits))
		#BSkyFormat(bl)
		
		# BSkyFormat(as.numeric(violators$combined_violation_indices))
		# BSkyFormat(which(names(statistics) %in% violators$combined_violation_indices))
		
		vr_named_indices = sort(setdiff(as.numeric(violators$combined_violation_indices), beyond_limits))
		vr <- sort(setdiff(as.numeric(which(names(statistics) %in% violators$combined_violation_indices)), bl))
		
		violations = list(beyond.limits = bl, beyond.limits.named.indices = beyond_limits, 
							violating.runs = vr, 
							violating.runs.named.indices = violators$combined_violation_indices)
		
		if(print == FALSE)
		{
			if(length(selcted_tests) > 0)
			{
				BSkyFormat(paste("Summary for the selected tests (", selected_test_str ,") performed for special causes in control charts"))
			}
			
			if(length(beyond_limits) > 0)
			{
				cat("\nBeyond", object$nsigmas, "σ limits - violating samples (", beyond_limits, ")\n")
			}
			else
			{
				cat("\nBeyond", object$nsigmas, "σ limits - no violating sample found\n")
			}
			
			if(length(selcted_tests) > 0)
			{
				if(length(violators$combined_violation_indices) > 0)
				{
					cat("\nCombind sample indices from all special cause tests performed - violating samples (", violators$combined_violation_indices, ")\n")
				}
				else
				{
					cat("\nCombind sample indices from all special cause tests performed - no violating sample found\n")
				}
			}
			
			if(debug == TRUE)
			{
				cat("\n====for debug only====\n")
				print(violations)
			}
		}

	return(invisible(violations))
}


violating.runs.indices <- function (object, test1 = TRUE, beyond.kdev.one.point = 3, test2 = TRUE, run.length = 9, 
									test3 = TRUE, increase.decrease.run.length = 6, test4 = TRUE, alternating.run.length = 14, 
									test5 = TRUE, beyond.plusone.2dev.run.length = 2, test6 = TRUE, beyond.plusone.1dev.run.length = 4,
									test7 = TRUE, within.1dev.run.length = 15, test8 = TRUE, beyond.1dev.run.length = 8, either.side = TRUE)
{
    
	# if ((missing(object)) | (!inherits(object, "qcc"))) 
			# stop("an object of class `qcc' is required")
	
	combined_violation_indices = c()
	violators <- list()
	
	type = object$type
	limits.func <- paste("limits.", type, sep = "")
	
    center <- object$center
    statistics <- c(object$statistics, object$newstats)
	
	# Test 1: One point more than 3σ from center line
	################################################################################################

	if(test1)
	{
		conf = beyond.kdev.one.point
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = object$sizes, conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		index.above.ucl <- seq(along = statistics)[statistics > ucl]
		index.below.lcl <- seq(along = statistics)[statistics < lcl]
		#violators$beyond.kdev.one.point.index = names(statistics)[c(index.above.ucl,index.below.lcl)]
		
		above_below_indices = sort(c(index.above.ucl,index.below.lcl))
		
		if(length(above_below_indices) == 1)
		{
			combined_violation_indices = c(combined_violation_indices, names(statistics)[c(index.above.ucl,index.below.lcl)])
		}
		
		# violators$beyond.kdev.one.point.index = matrix(statistics[c(index.above.ucl,index.below.lcl)], nrow =1)
		# dimnames(violators$beyond.kdev.one.point.index)[[2]] = names(statistics)[c(index.above.ucl,index.below.lcl)]
		# dimnames(violators$beyond.kdev.one.point.index)[[1]] = "sample value"
		
		
		
		violators$beyond.kdev.one.point.index = matrix(statistics[above_below_indices], nrow =1)
		dimnames(violators$beyond.kdev.one.point.index)[[2]] = names(statistics)[above_below_indices]
		dimnames(violators$beyond.kdev.one.point.index)[[1]] = "sample value"
	}
	
	
	# Test 2: Nine points in a row on the same side of the center line
    ################################################################################################

	if(test2)
	{
		diffs <- statistics - center
		diffs[diffs > 0] <- 1
		diffs[diffs < 0] <- -1
		runs <- rle(diffs)
		
		vruns <- rep(runs$lengths >= run.length, runs$lengths)
		vruns.above <- (vruns & (diffs > 0))
		vruns.below <- (vruns & (diffs < 0))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
			(rvruns.above$lengths - run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
			(rvruns.below$lengths - run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
		
		
		run.above.indices = numeric()
		run.below.indices = numeric()
		violators$run.above.indices = numeric()
		violators$run.below.indices = numeric()
		
		if (length(vbeg.above)) 
		{
			for (i in 1:length(vbeg.above)) run.above.indices <- c(run.above.indices,
				vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
			for (i in 1:length(vbeg.below)) run.below.indices <- c(run.below.indices,
				vbeg.below[i]:vend.below[i])
		}
		
		#violators$run.above.indices = as.numeric(names(statistics)[violators$run.above.indices])
		#violators$run.below.indices = as.numeric(names(statistics)[violators$run.below.indices])
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[run.above.indices])
		
		violators$run.above.indices = matrix(statistics[run.above.indices], nrow = 1)
		dimnames(violators$run.above.indices)[[2]] = names(statistics)[run.above.indices]
		dimnames(violators$run.above.indices)[[1]] = "sample value (above)"
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[run.below.indices])
		
		violators$run.below.indices = matrix(statistics[run.below.indices], nrow = 1)
		dimnames(violators$run.below.indices)[[2]] = names(statistics)[run.below.indices]
		dimnames(violators$run.below.indices)[[1]] = "sample value (below)"
	}
	
	
	# Test 3: Six points in a row, all increasing or all decreasing
	################################################################################################
	
	if(test3)
	{
		start.point.padded.statistics = c(0, statistics)
		diffs = diff(start.point.padded.statistics)
		diffs[(diffs > 0 | diffs == 0)]<- 1
		diffs[diffs < 0] <- -1
		runs <- rle(diffs)
		
		#increase.decrease.runs <- rep(runs$lengths >= (increase.decrease.run.length-1), runs$lengths)
		
		increase.decrease.runs = logical()
		
		if(length(runs$lengths) > 0)
		{
			for(x in 1:length(runs$lengths))
			{
				if(runs$lengths[x] >= (increase.decrease.run.length-1))
				{
					increase.decrease.runs = c(increase.decrease.runs, c(rep(FALSE, (increase.decrease.run.length-2))))
					increase.decrease.runs = c(increase.decrease.runs, c(rep(TRUE, (runs$lengths[x] - (increase.decrease.run.length-2)))))
				}
				else 
				{
					increase.decrease.runs = c(increase.decrease.runs, rep(FALSE, runs$lengths[x]))
				}
			}
		}
		
		
		#violators$increase.decrease.run.indices = as.numeric(names(statistics)[c(increase.decrease.runs)])
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[increase.decrease.runs])
		
		violators$increase.decrease.run.indices = matrix(statistics[increase.decrease.runs], nrow = 1)
		dimnames(violators$increase.decrease.run.indices)[[2]] = names(statistics)[increase.decrease.runs]
		dimnames(violators$increase.decrease.run.indices)[[1]] = "sample value"
	}


	# Test 4: Fourteen points in a row, alternating up and down
	#########################################################################################################
	
	if(test4)
	{
		start.point.padded.statistics = c(statistics, (statistics[length(statistics)]+1) )
		diffs = diff(start.point.padded.statistics)
		diffs[(diffs > 0 | diffs == 0)]<- 1
		diffs[diffs < 0] <- -1
		runs <- rle(diffs)
		
		#alternating.runs1 <- rep(runs$lengths >= 2, runs$lengths)
		
		alternating.runs = logical()
		
		if(length(runs$lengths) > 0)
		{
			for(x in 1:length(runs$lengths))
			{
				if(runs$lengths[x] >= 2)
				{
					alternating.runs = c(alternating.runs, FALSE)
					alternating.runs = c(alternating.runs, c(rep(TRUE, (runs$lengths[x] - 1))))
				}
				else 
				{
					alternating.runs = c(alternating.runs, rep(FALSE, runs$lengths[x]))
				}
			}
		}
		
		names(alternating.runs) = names(statistics)
		
		#BSkyFormat(rbind(alternating.runs,alternating.runs1))
		
		violators$alternate.run.indices = numeric()
		alternate.run.indices = numeric()
		
		for(i in 1:length(alternating.runs))
		{
			if((i+ (alternating.run.length -2)) <= length(alternating.runs))
			{
				run.broken = FALSE
				
				for(j in i:(i+ (alternating.run.length -2)))
				{
					if(alternating.runs[j] == TRUE)
					{
						run.broken = TRUE
					}
				}
				
				if(run.broken == FALSE)
				{
					alternate.run.indices = c(alternate.run.indices, (i+ (alternating.run.length -1)))
				}
			}
		}
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[alternate.run.indices])
		
		violators$alternate.run.indices = matrix(statistics[alternate.run.indices], nrow = 1)
		dimnames(violators$alternate.run.indices)[[2]] = names(statistics)[alternate.run.indices]
		dimnames(violators$alternate.run.indices)[[1]] = "sample value"
		
		
		# alternating.runs.count = rle(alternating.runs)
		# names(alternating.runs.count$lengths) = cumsum(alternating.runs.count$lengths)
		# v.alternating.runs = alternating.runs.count$length[(alternating.runs.count$length > (alternating.run.length)) & alternating.runs.count$values == FALSE]
		
		# violators$alternate.run.indices = list()
		
		# if(length(v.alternating.runs))
		# {
			# for(i in 1: length(v.alternating.runs))
			# {
				# from.index = as.numeric(names(v.alternating.runs)[i]) - v.alternating.runs[i]
				
				# if(from.index == 0) from.index = 1
				
				# to.index = as.numeric(names(v.alternating.runs)[i])
				
				# #violators$alternate.run.indices = c(violators$alternate.run.indices, list(as.numeric(names(statistics)[from.index:to.index])))
				
				# combined_violation_indices = c(combined_violation_indices, names(statistics)[from.index:to.index])
				
				# indices_matrix = matrix(statistics[from.index:to.index], nrow = 1)
				# dimnames(indices_matrix)[[2]] = names(statistics)[from.index:to.index]
				# dimnames(indices_matrix)[[1]] = "sample value"
				# violators$alternate.run.indices = c(violators$alternate.run.indices, list(indices_matrix))
			# }
		# }
	}

	# Test 5: Two out of three points more than 2σ from the center line (same side)
	################################################################################################
	
	if(test5)
	{
		conf = 2
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = object$sizes, conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		diffs <- statistics - center
		diffs[diffs > 0 & statistics > ucl ] <- 1
		diffs[diffs > 0 & statistics <= ucl ] <- 9
		diffs[diffs < 0 & statistics < lcl ] <- -1
		diffs[diffs < 0 & statistics >= lcl ] <- -9
		
		names(diffs) = names(statistics)
		
		plusone.2dev.above.indices = numeric()
		plusone.2dev.below.indices = numeric()
		violators$beyond.plusone.2dev.above.indices = numeric()
		violators$beyond.plusone.2dev.below.indices = numeric()
		
		num.stat.points = length(diffs)
		
		for(i in 1:num.stat.points)
		{
			if((i+ beyond.plusone.2dev.run.length) <= num.stat.points)
			{
				beyond.plusone.2dev.above.indices = which(diffs[i:(i+beyond.plusone.2dev.run.length)] == 1)
				beyond.plusone.2dev.below.indices = which(diffs[i:(i+beyond.plusone.2dev.run.length)] == -1)
				
				if(length(beyond.plusone.2dev.above.indices) >= beyond.plusone.2dev.run.length)
				{
					plusone.2dev.above.indices = c(plusone.2dev.above.indices, names(diffs[i:(i+beyond.plusone.2dev.run.length)])[beyond.plusone.2dev.above.indices])
				}
				
				if(length(beyond.plusone.2dev.below.indices) >= beyond.plusone.2dev.run.length)
				{
					plusone.2dev.below.indices = c(plusone.2dev.below.indices, beyond.plusone.2dev.below.indices)
				}
			}
		}

		plusone.2dev.above.indices = unique(plusone.2dev.above.indices)
		plusone.2dev.below.indices = unique(plusone.2dev.below.indices)
		
		combined_violation_indices = c(combined_violation_indices, plusone.2dev.above.indices)
		
		violators$beyond.plusone.2dev.above.indices = matrix(statistics[plusone.2dev.above.indices], nrow = 1)
		dimnames(violators$beyond.plusone.2dev.above.indices)[[2]] = plusone.2dev.above.indices
		dimnames(violators$beyond.plusone.2dev.above.indices)[[1]] = "sample value"
		
		combined_violation_indices = c(combined_violation_indices, plusone.2dev.below.indices)
		
		violators$beyond.plusone.2dev.below.indices = matrix(statistics[plusone.2dev.below.indices], nrow = 1)
		dimnames(violators$beyond.plusone.2dev.below.indices)[[2]] = plusone.2dev.below.indices
		dimnames(violators$beyond.plusone.2dev.below.indices)[[1]] = "sample value"
	}
		

	# Test 6: Four out of five points more than 1σ from center line (same side)
	#########################################################################################################
	
	if(test6)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = object$sizes, conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		diffs <- statistics - center
		diffs[diffs > 0 & statistics > ucl ] <- 1
		diffs[diffs > 0 & statistics <= ucl ] <- 9
		diffs[diffs < 0 & statistics < lcl ] <- -1
		diffs[diffs < 0 & statistics >= lcl ] <- -9
		
		names(diffs) = names(statistics)
		
		plusone.1dev.above.indices = numeric()
		plusone.1dev.below.indices = numeric()
		
		violators$beyond.plusone.1dev.above.indices = numeric()
		violators$beyond.plusone.1dev.below.indices = numeric()
		
		num.stat.points = length(diffs)
		
		for(i in 1:num.stat.points)
		{
			if((i+ beyond.plusone.1dev.run.length) <= num.stat.points)
			{
				beyond.plusone.1dev.above.indices = which(diffs[i:(i+beyond.plusone.1dev.run.length)] == 1)
				beyond.plusone.1dev.below.indices = which(diffs[i:(i+beyond.plusone.1dev.run.length)] == -1)
				
				if(length(beyond.plusone.1dev.above.indices) >= beyond.plusone.1dev.run.length)
				{
					plusone.1dev.above.indices = c(plusone.1dev.above.indices, names(diffs[i:(i+beyond.plusone.1dev.run.length)])[beyond.plusone.1dev.above.indices])
				}
				
				if(length(beyond.plusone.1dev.below.indices) >= beyond.plusone.1dev.run.length)
				{
					plusone.1dev.below.indices = c(plusone.1dev.below.indices, beyond.plusone.1dev.below.indices)
				}
			}
		}

		#violators$beyond.plusone.1dev.above.indices = unique(violators$beyond.plusone.1dev.above.indices)
		#violators$beyond.plusone.1dev.below.indices = unique(violators$beyond.plusone.1dev.below.indices)
		
		plusone.1dev.above.indices = unique(plusone.1dev.above.indices)
		plusone.1dev.below.indices = unique(plusone.1dev.below.indices)
		
		combined_violation_indices = c(combined_violation_indices, plusone.1dev.above.indices)
		
		violators$beyond.plusone.1dev.above.indices = matrix(statistics[plusone.1dev.above.indices], nrow = 1)
		dimnames(violators$beyond.plusone.1dev.above.indices)[[2]] = plusone.1dev.above.indices
		dimnames(violators$beyond.plusone.1dev.above.indices)[[1]] = "sample value"
		
		combined_violation_indices = c(combined_violation_indices, plusone.1dev.below.indices)
		
		violators$beyond.plusone.1dev.below.indices = matrix(statistics[plusone.1dev.below.indices], nrow = 1)
		dimnames(violators$beyond.plusone.1dev.below.indices)[[2]] = plusone.1dev.below.indices
		dimnames(violators$beyond.plusone.1dev.below.indices)[[1]] = "sample value"
	}
		
	
	# Test 7: Fifteen points in a row within 1σ of center line (either side)
	##################################################################################################
	
	if(test7)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = object$sizes, conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		if(either.side == TRUE)
		{
			diffs <- statistics - center
			diffs[statistics <= ucl & statistics >= lcl] <- 1
			diffs[diffs > 0 & statistics > ucl ] <- 9
			diffs[diffs < 0 & statistics < lcl ] <- -9
		}
		else
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics <= ucl ] <- 1
			diffs[diffs > 0 & statistics > ucl ] <- 9
			diffs[diffs < 0 & statistics >= lcl ] <- -1
			diffs[diffs < 0 & statistics < lcl ] <- -9
		}

		runs <- rle(diffs)
			
		vruns <- rep((runs$lengths >= within.1dev.run.length & abs(runs$values) != 9), runs$lengths)
		vruns.above <- (vruns & (diffs > 0 & diffs != 9))
		vruns.below <- (vruns & (diffs < 0 & diffs != -9))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
				(rvruns.above$lengths - within.1dev.run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
				(rvruns.below$lengths - within.1dev.run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
			
		within.1dev.above.indices = numeric()
		within.1dev.below.indices = numeric()
		
		violators$within.1dev.above.indices = numeric()
		violators$within.1dev.below.indices = numeric()
			
		if (length(vbeg.above)) 
		{
				for (i in 1:length(vbeg.above)) within.1dev.above.indices <- c(within.1dev.above.indices,
					vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
				for (i in 1:length(vbeg.below)) within.1dev.below.indices <- c(within.1dev.below.indices,
					vbeg.below[i]:vend.below[i])
		}
			
		#violators$within.1dev.above.indices = as.numeric(names(statistics)[violators$within.1dev.above.indices])
		#violators$within.1dev.below.indices = as.numeric(names(statistics)[violators$within.1dev.below.indices])
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[within.1dev.above.indices])
		
		violators$within.1dev.above.indices = matrix(statistics[within.1dev.above.indices], nrow = 1)
		dimnames(violators$within.1dev.above.indices)[[2]] = names(statistics)[within.1dev.above.indices]
		dimnames(violators$within.1dev.above.indices)[[1]] = "sample value"
		
		# print(within.1dev.above.indices)
		# print(violators$within.1dev.above.indices)
		# print(dim(violators$within.1dev.above.indices))
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[within.1dev.below.indices])
		
		violators$within.1dev.below.indices = matrix(statistics[within.1dev.below.indices], nrow = 1)
		dimnames(violators$within.1dev.below.indices)[[2]] = names(statistics)[within.1dev.below.indices]
		dimnames(violators$within.1dev.below.indices)[[1]] = "sample value"
	}
		
	
	# Test 8: Eight points in a row more than 1σ from center line (either side)
	################################################################################################
	
	if(test8)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = object$sizes, conf = conf))
		
		
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		if(either.side == TRUE)
		{
			diffs <- statistics - center
			diffs[statistics > ucl | statistics < lcl] <- 1
			diffs[diffs > 0 & statistics <= ucl ] <- 9
			diffs[diffs < 0 & statistics >= lcl ] <- -9
		}
		else
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics > ucl ] <- 1
			diffs[diffs > 0 & statistics <= ucl ] <- 9
			diffs[diffs < 0 & statistics < lcl ] <- -1
			diffs[diffs < 0 & statistics >= lcl ] <- -9
		}

		runs <- rle(diffs)
		
		vruns <- rep((runs$lengths >= beyond.1dev.run.length & abs(runs$values) != 9), runs$lengths)
		vruns.above <- (vruns & (diffs > 0 & diffs != 9))
		vruns.below <- (vruns & (diffs < 0 & diffs != -9))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
			(rvruns.above$lengths - beyond.1dev.run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
			(rvruns.below$lengths - beyond.1dev.run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
		
		beyond.1dev.above.indices = numeric()
		beyond.1dev.below.indices = numeric()
		
		violators$beyond.1dev.above.indices = numeric()
		violators$beyond.1dev.below.indices = numeric()
		
		if (length(vbeg.above)) 
		{
			for (i in 1:length(vbeg.above)) beyond.1dev.above.indices <- c(beyond.1dev.above.indices,
				vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
			for (i in 1:length(vbeg.below)) beyond.1dev.below.indices <- c(beyond.1dev.below.indices,
				vbeg.below[i]:vend.below[i])
		}
		
		#violators$beyond.1dev.above.indices = as.numeric(names(statistics)[violators$beyond.1dev.above.indices])
		#violators$beyond.1dev.below.indices = as.numeric(names(statistics)[violators$beyond.1dev.below.indices])
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[beyond.1dev.above.indices])
		
		violators$beyond.1dev.above.indices = matrix(statistics[beyond.1dev.above.indices], nrow = 1)
		dimnames(violators$beyond.1dev.above.indices)[[2]] = names(statistics)[beyond.1dev.above.indices]
		dimnames(violators$beyond.1dev.above.indices)[[1]] = "sample value"
		
		combined_violation_indices = c(combined_violation_indices, names(statistics)[beyond.1dev.below.indices])
		
		violators$beyond.1dev.below.indices = matrix(statistics[beyond.1dev.below.indices], nrow = 1)
		dimnames(violators$beyond.1dev.below.indices)[[2]] = names(statistics)[beyond.1dev.below.indices]
		dimnames(violators$beyond.1dev.below.indices)[[1]] = "sample value"
	}
		
	
	#########################################################################################################
	
	violators$combined_violation_indices = sort(as.numeric(unique(combined_violation_indices)))
	
    return(invisible(violators))
}


