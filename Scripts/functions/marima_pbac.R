marima.pbac <- function(DATA = NULL, ar.pattern = NULL, ma.pattern = NULL, 
    means = 1, max.iter = 50, penalty = 0, weight = 0.33, Plot = "none", 
    Check = FALSE) 
{
    Test <- FALSE
    kvar <- min(dim(DATA))
    till = 6
    if (is.null(ar.pattern) & is.null(ma.pattern)) {
        cat("Neither ar- nor ma-part of model specified. \n")
    }
    if (is.null(ar.pattern)) {
        ar.pattern = array(c(diag(kvar), 0 * diag(kvar)), dim = c(kvar, 
            kvar, 2))
    }
    if (is.null(ma.pattern)) {
        ma.pattern = array(c(diag(kvar), 0 * diag(kvar)), dim = c(kvar, 
            kvar, 2))
        max.iter <- 3
        till <- 1
        weight <- 0
    }
    AR <- ar.pattern
    MA <- ma.pattern
    max.iter <- round(max.iter)
    D <- dim(DATA)
    if (D[1] < D[2]) {
        DATA <- t(DATA)
    }
    case = which(complete.cases(DATA))
    All <- 1:max(dim(DATA))
    DATA <- DATA[case, ]
    Leftout <- setdiff(All, case)
    if (length(Leftout) <= 0) {
        cat("All cases in data, ", min(case), " to ", max(case), 
            " accepted for completeness.\n")
    }
    if (length(Leftout) > 0) {
        cat("Cases ", Leftout, " left out for marima analysis. \n")
    }
    D <- dim(DATA)
    N <- D[1]
    kvar <- D[2]
    rownames(DATA) <- as.character(1:N)
    randoms <- which(rowSums(matrix(abs(c(AR, MA)), nrow = kvar)) > 
        2)
    non.randoms <- which(rowSums(matrix(abs(c(AR, MA)), nrow = kvar)) == 
        2)
    xmean <- means
    if (length(xmean) == 1) {
        if (xmean == 1) {
            means <- rep(1, kvar)
        }
    }
    if (length(xmean) == 1) {
        if (xmean != 1) {
            means <- rep(0, kvar)
        }
    }
    if (Check) {
        cat("Control printout (Check=TRUE (default)). \n")
        cat("----------------------\n")
        cat("Calling marima. Data dimensions (kvar,N) = ", kvar, 
            N, "\n")
        cat("Coefficient polynomial dimensions = ", dim(AR), 
            " and ", dim(MA), "\n")
        cat("including leading unity matrix. \n")
        if (is.null(ar.pattern)) {
            cat("no ar.pattern specified \n")
        }
        if (is.null(ma.pattern)) {
            cat("no ma.pattern specified \n")
        }
        if (!is.null(AR)) {
            cat("ar.pattern= \n")
            print(AR)
        }
        if (!is.null(MA)) {
            cat("ma.pattern= \n")
            print(MA)
        }
        cat("Start of data (5 first observations): \n")
        print(DATA[1:5, ])
        cat(" ... \n")
        cat("End of data (5 last observations): \n")
        print(DATA[(N - 4):N, ])
        cat(" \n")
        cat(" Calling parameters in use: \n")
        cat(" max.iter = ", max.iter, ", means = ", means, ",\n        penalty = ", 
            penalty, ", \n")
        cat(" weight = ", weight, ", Plot = ", Plot, ", Check = ", 
            Check, " \n")
        cat(" The above printout can be suppressed ", "by calling with Check = FALSE. \n ")
    }
    ARmodel <- matrix(0, nrow = kvar, ncol = (kvar * dim(AR)[3]))
    MAmodel <- matrix(0, nrow = kvar, ncol = (kvar * dim(MA)[3]))
    for (i in 1:kvar) {
        ARmodel[i, which(AR[i, , ] > 0)] <- which(AR[i, , ] > 
            0)
        MAmodel[i, which(MA[i, , ] > 0)] <- which(MA[i, , ] > 
            0)
    }
    if (Test) {
        cat("ARmodel & MAmodel as constructed: \n")
        print(ARmodel)
        print(MAmodel)
        readline("0.4: Press <return to continue")
    }
    arnam <- matrix(paste("ar", ARmodel, sep = ""), nrow = kvar)
    manam <- matrix(paste("ma", MAmodel, sep = ""), nrow = kvar)
    if (Test) {
        cat("arnam & manam as constructed: \n")
        cat("dim(arnam),dim(manam) \n")
        print(arnam)
        print(manam)
        readline("0.5: Press >return to continue ")
    }
    On <- rep(0, kvar)
    for (i in 1:kvar) {
        if (sum(AR[i, , ]) > 1) {
            On[i] <- i
        }
        if (sum(AR[, i, ]) > 1) {
            On[i] <- i
        }
    }
    On <- On[On > 0]
    cases1 <- which(complete.cases(DATA[, On]))
    rownames(DATA) <- as.character(1:dim(DATA)[1])
    DATA <- DATA[cases1, ]
    D <- dim(DATA)
    X <- lagged.data(DATA = DATA, AR = AR, MA = MA, init = TRUE, 
        means = xmean)
    averages <- X$averages
    if (Test) {
        cat("About X \n")
        cat(names(X), " \n")
        cat("Averages =", X$averages, "\n")
        cat("X$ardata og X$madata = \n")
        na <- dim(X$ardata)[1]
        nm <- dim(X$madata)[1]
        print(X$ardata[c((1:8), ((na - 7):na)), ])
        print(X$madata[c((1:8), ((nm - 7):nm)), ])
        readline("1.5: Press <return to continue")
    }
    N <- dim(X$ardata)[1]
    SAR <- short.form(AR, "", tail = TRUE)
    ARlags <- as.numeric(dimnames(SAR)[[3]][])
    L <- dim(SAR)[3]
    SAM <- short.form(MA, "", tail = TRUE)
    MAlags <- as.numeric(dimnames(SAM)[[3]][])
    M <- dim(SAM)[3]
    if (Test) {
        cat("SAR , ARlags , SAM , MAlags: \n")
        print(SAR)
        print(ARlags)
        print(SAM)
        print(MAlags)
        readline("2.0: Press >return to continue")
    }
    stt <- ARlags * kvar + 1
    stp <- ARlags * kvar + kvar
    for (i in 1:length(stt)) {
        if (i == 1) {
            arcols <- c(stt[i]:stp[i])
        }
        if (i > 1) {
            arcols <- c(arcols, c(stt[i]:stp[i]))
        }
    }
    stt <- MAlags * kvar + 1
    stp <- MAlags * kvar + kvar
    for (i in 1:length(stt)) {
        if (i == 1) {
            macols <- c(stt[i]:stp[i])
        }
        if (i > 1) {
            macols <- c(macols, c(stt[i]:stp[i]))
        }
    }
    if (Test) {
        cat("arcols = ", arcols, "\n")
        cat("macols = ", macols, "\n")
        readline("2.5: Press <-return to continue")
    }
    ARDATA <- fill.out(DATAarray = array(X$ardata, dim = c(N, 
        kvar, L)), SAR = SAR)
    MADATA <- fill.out(DATAarray = array(X$madata, dim = c(N, 
        kvar, M)), SAR = SAM)
    ARDATA <- matrix(ARDATA, nrow = N)
    MADATA <- matrix(MADATA, nrow = N)
    rownames(ARDATA) <- rownames(DATA)
    rownames(MADATA) <- rownames(DATA)
    colnames(ARDATA) <- paste("ar", arcols, sep = "")
    colnames(MADATA) <- paste("ma", macols, sep = "")
    if (Test) {
        cat("SAR and SAM \n")
        cat(dim(SAR), "..", dim(SAM), " \n")
        print(SAR)
        print(SAM)
        readline("3.14: Press <return to continue")
        cat("ARDATA & MADATA \n")
        na <- dim(ARDATA)[1]
        nm <- dim(MADATA)[1]
        print(ARDATA[c((1:8), ((na - 7):na)), ])
        cat("..... \n")
        print(MADATA[c((1:8), ((nm - 7):nm)), ])
        readline("3.15: Press <return to continue")
    }
    On2 <- matrix(rep(On, L), ncol = L)
    if (L > 1) {
        for (i in 2:L) {
            On2[, i] <- On2[, 1] + (i - 1) * kvar
        }
    }
    cases <- which(complete.cases(ARDATA[, c(On2)]))
    ARDATA <- ARDATA[cases, ]
    MADATA <- MADATA[cases, ]
    N <- length(cases)
    if (Test) {
        cat("colnames ARDATA", colnames(ARDATA), "\n")
        cat("colnames MADATA", colnames(MADATA), "\n")
        readline("4.0: Press <return to continue")
    }
    DA <- get.models(ARDATA = ARDATA, AR = AR)
    DM <- get.models(ARDATA = MADATA, AR = MA)
    if (Test) {
        cat("DA  and  DM \n")
        print(DA)
        print(DM)
        readline("4.5: Press <return to continue")
    }
    randoms <- randoms
    trace <- 0
    log.det <- 0
    NAM = ""
    Iter1 <- round(max.iter/3)
    Iter3 <- Iter1 + till
    for (iterate in 1:max.iter) {
        for (i in 1:kvar) {
            colar <- colnames(ARDATA)
            colma <- colnames(MADATA)
            varx <- DA[i, ]
            varx <- varx[varx > 1]
            vare <- DM[i, ]
            vare <- vare[vare > 1]
            MO <- lm.form(i, colnames(ARDATA), zero = T)
            if (Test & iterate == 3) {
                cat(as.character(MO), "\n")
                readline("4.7: Press <return to continue")
            }
            MO <- lm.adds(MO, varx, colar)
            if (Test & iterate == 3) {
                cat(as.character(MO), "\n")
                readline("4.80: Press <return to continue")
            }
            MO <- lm.adds(MO, vare, colma)
            if (Test & iterate == 3) {
                cat(as.character(MO), "\n")
                readline("4.90: Press <return to continue")
            }
            if (iterate > Iter3 & penalty > 0) {
                MO <- NAM[i]
            }
            if (Test & iterate == Iter3 + 3) {
                cat("NAM[i]= ", as.character(NAM[i]), "\n")
                cat("MO=     ", as.character(MO), "\n")
                readline("4.92: Press <return to continue")
            }
            MOD <- lm(MO, data = data.frame(ARDATA, MADATA))
            if (Test & iterate == 3) {
                cat("ar-colnames=", colar, "\n")
                cat("ma-colnames=", colma, "\n")
                cat("iterate,i", iterate, i, "\n")
                cat(as.character(MO), "\n")
                readline("5: Press <return to continue")
            }
            summary(MOD)
            if (penalty > 1e-04) {
                if (iterate > Iter1) {
                  if (iterate >= Iter1 & iterate <= Iter3 & penalty > 
                    0) {
                    MOD <- step(MOD, direction = "backward", 
                      k = penalty, trace = FALSE)
                  }
                  if (iterate == Iter3) {
                    NAM[i] <- as.character(MOD$call[2])
                  }
                }
            }
            if (iterate == max.iter) {
                if (i == 1) {
                  ar.estimates <- matrix(AR * 0, nrow = kvar)
                  ar.estimates[1:kvar, 1:kvar] <- diag(kvar)
                  ma.estimates <- matrix(MA * 0, nrow = kvar)
                  ma.estimates[1:kvar, 1:kvar] <- diag(kvar)
                  ar.fvalues <- ar.estimates
                  ma.fvalues <- ma.estimates
                  ar.pvalues <- ar.estimates
                  ma.pvalues <- ma.estimates
                }
                varia <- names(MOD$coefficients)
                if (Test) {
                  cat("varia=names(MOD$coefficients=", varia, 
                    "\n")
                  readline("6: Press <return to continue")
                }
                if (length(varia) > 0) {
                  return(MOD)
                  types <- substr(varia, 1, 2)
                  numbers <- as.numeric(substr(varia, 3, 10))
                  ars <- which(types == "ar")
                  mas <- which(types == "ma")
                  ar.estimates[i, numbers[ars]] <- -MOD$coefficients[ars]
                  ma.estimates[i, numbers[mas]] <- +MOD$coefficients[mas]
                  f.values <- summary(MOD)[[4]][, 3]
                  f.values <- f.values^2
                  ar.fvalues[i, numbers[ars]] <- f.values[ars]
                  ma.fvalues[i, numbers[mas]] <- f.values[mas]
                  p.values <- summary(MOD)[[4]][, 4]
                  ar.pvalues[i, numbers[ars]] <- p.values[ars]
                  ma.pvalues[i, numbers[mas]] <- p.values[mas]
                }
                if (iterate == max.iter) {
                  if (i == kvar) {
                    ar.estimates <- array(ar.estimates, dim = dim(AR))
                    ma.estimates <- array(ma.estimates, dim = dim(MA))
                    ar.fvalues <- array(ar.fvalues, dim = dim(AR))
                    ma.fvalues <- array(ma.fvalues, dim = dim(MA))
                    ar.pvalues <- array(ar.pvalues, dim = dim(AR))
                    ma.pvalues <- array(ma.pvalues, dim = dim(MA))
                  }
                }
            }
            res <- MOD$residual
            MADATA[, i] <- weight * MADATA[, i] + (1 - weight) * 
                res
            log.det[iterate] <- log(det(cov(MADATA[, randoms])))
            trace[iterate] <- sum(diag(cov(MADATA[, 1:kvar])))
            MADATA <- fill.out(DATAarray = array(MADATA, dim = c(N, 
                kvar, M)), SAR = SAM)
            MADATA <- matrix(MADATA, nrow = N)
            colnames(MADATA) <- colma
        }
    }
    rownames(MADATA) <- rownames(ARDATA)
    fitted <- ARDATA[, 1:kvar] - MADATA[, 1:kvar]
    for (i in 1:kvar) {
        fitted[, i] <- fitted[, i] + averages[i] * means[i]
    }
    colnames(fitted) <- paste("y", 1:kvar, sep = "")
    fitted <- t(fitted)
    residuals <- t(MADATA[, 1:kvar])
    rownames(residuals) <- paste("u", 1:kvar, sep = "")
    covu <- cov(MADATA[, 1:kvar])
    covy <- cov(ARDATA[, 1:kvar])
    colnames(covu) <- rownames(residuals)
    rownames(covu) <- rownames(residuals)
    colnames(covy) <- paste("y", c(1:kvar), sep = "")
    rownames(covy) <- colnames(covy)
    mains = "Residual covariance matrix trace"
    if (Plot == "trace" & iterate >= 3) {
        plot(trace[1:max.iter], main = mains, xlab = "No. of iterations", 
            ylab = "Computed trace", type = "l")
        grid(col = "blue")
    }
    mains = "log(det(residual covariance matrix))"
    if (Plot == "log.det" & iterate >= 3) {
        plot(log.det[1:max.iter], main = mains, xlab = "No. of iterations", 
            ylab = "log(det(residual covariance matrix))", type = "l")
        grid(col = "blue")
    }
    used.cases <- as.numeric(colnames(residuals))
    out.ar.pattern <- abs(ar.estimates)
    out.ar.pattern[which(out.ar.pattern > 0)] <- 1
    out.ma.pattern <- abs(ma.estimates)
    out.ma.pattern[which(out.ma.pattern > 0)] <- 1
    am <- averages * means
    Constant <- am
    lar <- length(c(ar.estimates))/(kvar * kvar)
    if (lar > 1) {
        for (i in 2:lar) {
            Constant <- Constant + ar.estimates[, , i] %*% am
        }
    }
    obj <- list(N = N, DATA = DATA, kvar = kvar, ar.estimates = ar.estimates, 
        ma.estimates = ma.estimates, Constant = Constant, ar.fvalues = ar.fvalues, 
        ma.fvalues = ma.fvalues, ar.pvalues = ar.pvalues, ma.pvalues = ma.pvalues, 
        residuals = residuals, fitted = fitted, resid.cov = covu, 
        data.cov = covy, averages = averages, mean.pattern = means, 
        call.ar.pattern = AR, call.ma.pattern = MA, out.ar.pattern = out.ar.pattern, 
        out.ma.pattern = out.ma.pattern, max.iter = max.iter, 
        penalty = penalty, weight = weight, used.cases = used.cases, 
        trace = trace, log.det = log.det, randoms = randoms)
    class(obj) <- "marima"
    obj
}
