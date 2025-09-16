## Fitting the basic local independence model (BLIM) by MDML


#' Basic Local Independence Models (BLIMs)
#' 
#' Fits a basic local independence model (BLIM) for probabilistic knowledge
#' structures by minimum discrepancy maximum likelihood estimation.
#' 
#' See Doignon and Falmagne (1999) for details on the basic local independence
#' model (BLIM) for probabilistic knowledge structures.
#' 
#' Minimum discrepancy (MD) minimizes the number of expected response errors
#' (careless errors or lucky guesses). Maximum likelihood maximizes the
#' likelihood, possibly at the expense of inflating the error and guessing
#' parameters.  Minimum discrepancy maximum likelihood (MDML) maximizes the
#' likelihood subject to the constraint of minimum response errors.  See Heller
#' and Wickelmaier (2013) for details on the parameter estimation methods.
#' 
#' If \code{randinit} is \code{TRUE}, initial parameter values are sampled
#' uniformly with the constraint \code{beta + eta < 1} (Weisstein, 2013) for
#' the error parameters, and with \code{sum(P.K) == 1} (Rubin, 1981) for the
#' probabilities of knowledge states. Setting \code{randinit} to \code{TRUE}
#' overrides any values given in the \code{P.K}, \code{beta}, and \code{eta}
#' arguments.
#' 
#' The degrees of freedom in the goodness-of-fit test are calculated as number
#' of possible response patterns minus one or number of respondents, whichever
#' is smaller, minus number of parameters.
#' 
#' \code{blimMD} uses minimum discrepancy estimation only.  Apart from the
#' hyperbolic inclusion rules, all of its functionality is also provided by
#' \code{blim}.  It may be removed in the future.
#' 
#' @aliases blim blimMD anova.blim coef.blim deviance.blim logLik.blim
#' nobs.blim
#' @param K a state-by-problem indicator matrix representing the knowledge
#' structure.  An element is one if the problem is contained in the state, and
#' else zero.
#' @param N.R a (named) vector of absolute frequencies of response patterns.
#' @param method \code{MD} for minimum discrepancy estimation, \code{ML} for
#' maximum likelihood estimation, \code{MDML} for minimum discrepancy maximum
#' likelihood estimation.
#' @param R a person-by-problem indicator matrix of unique response patterns.
#' Per default inferred from the names of \code{N.R}.
#' @param P.K the vector of initial parameter values for probabilities of
#' knowledge states.
#' @param beta,eta vectors of initial parameter values for probabilities of a
#' careless error and a lucky guess, respectively.
#' @param betafix,etafix vectors of fixed error and guessing parameter values;
#' \code{NA} indicates a free parameter.
#' @param betaequal,etaequal lists of vectors of problem indices; each vector
#' represents an equivalence class: it contains the indices of problems for
#' which the error or guessing parameters are constrained to be equal.  (See
#' Examples.)
#' @param randinit logical, if \code{TRUE} then initial parameter values are
#' sampled uniformly with constraints.  (See Details.)
#' @param incradius include knowledge states of distance from the minimum
#' discrepant states less than or equal to \code{incradius}.
#' @param tol tolerance, stopping criterion for iteration.
#' @param maxiter the maximum number of iterations.
#' @param zeropad the maximum number of items for which an incomplete
#' \code{N.R} vector is completed and padded with zeros.
#' @param incrule inclusion rule for knowledge states.  (See Details.)
#' @param m exponent for hyperbolic inclusion rules.
#' @param object an object of class \code{blim}, typically the result of a call
#' to \code{blim}.
#' @param test should the p-values of the chi-square distributions be reported?
#' @param \dots additional arguments passed to other methods.
#' @return An object of class \code{blim} having the following components:
#' \item{discrepancy}{the mean minimum discrepancy between response patterns
#' and knowledge states.} \item{P.K}{the vector of estimated parameter values
#' for probabilities of knowledge states.} \item{beta}{the vector of estimated
#' parameter values for probabilities of a careless error.} \item{eta}{the
#' vector of estimated parameter values for probabilities of a lucky guess.}
#' \item{disc.tab}{the minimum discrepancy distribution.} \item{K}{the
#' knowledge structure.} \item{N.R}{the vector of frequencies of response
#' patterns.} \item{nitems}{the number of items.} \item{nstates}{the number of
#' knowledge states.} \item{npatterns}{the number of response patterns.}
#' \item{ntotal}{the number of respondents.} \item{nerror}{the number of
#' response errors.} \item{npar}{the number of parameters.} \item{method}{the
#' parameter estimation method.} \item{iter}{the number of iterations needed.}
#' \item{loglik}{the log-likelihood.} \item{fitted.values}{the fitted response
#' frequencies.} \item{goodness.of.fit}{the goodness of fit statistic including
#' the likelihood ratio fitted vs. saturated model (G2), the degrees of
#' freedom, and the p-value of the corresponding chi-square distribution.  (See
#' Details.)}
#' @seealso \code{\link{simulate.blim}}, \code{\link{plot.blim}},
#' \code{\link{residuals.blim}}, \code{\link{logLik.blim}},
#' \code{\link{delineate}}, \code{\link{jacobian}}, \code{\link{endm}},
#' \code{\link{probability}}, \code{\link{chess}}.
#' @references Doignon, J.-P., & Falmagne, J.-C. (1999).  \emph{Knowledge
#' spaces}. Berlin: Springer.
#' 
#' Heller, J., & Wickelmaier, F. (2013).  Minimum discrepancy estimation in
#' probabilistic knowledge structures.  \emph{Electronic Notes in Discrete
#' Mathematics}, \bold{42}, 49--56.
#' c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1016/j.endm.2013.05.145")\Sexpr{tools:::Rd_expr_doi("10.1016/j.endm.2013.05.145")}
#' 
#' Rubin, D.B. (1981). The Bayesian bootstrap.  \emph{The Annals of
#' Statistics}, \bold{9}(1), 130--134.
#' c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1214/aos/1176345338")\Sexpr{tools:::Rd_expr_doi("10.1214/aos/1176345338")}
#' 
#' Weisstein, E.W. (2013, August 29). Triangle point picking.  In
#' \emph{MathWorld -- A Wolfram Web Resource}.  Retrieved from
#' \url{https://mathworld.wolfram.com/TrianglePointPicking.html}.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#' K   <- DoignonFalmagne7$K    # knowledge structure
#' N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns
#' 
#' ## Fit basic local independence model (BLIM) by different methods
#' blim(K, N.R, method = "MD")    # minimum discrepancy estimation
#' blim(K, N.R, method = "ML")    # maximum likelihood estimation by EM
#' blim(K, N.R, method = "MDML")  # MDML estimation
#' 
#' ## Parameter restrictions: beta_a = beta_b = beta_d, beta_c = beta_e
#' ##                          eta_a =  eta_b = 0.1
#' m1 <- blim(K, N.R, method = "ML",
#'            betaequal = list(c(1, 2, 4), c(3, 5)),
#'               etafix = c(0.1, 0.1, NA, NA, NA))
#' m2 <- blim(K, N.R, method = "ML")
#' anova(m1, m2)
#' 
#' ## See ?endm, ?probability, and ?chess for further examples.
#' 
#' @export blim
blim <- function(K, N.R, method = c("MD", "ML", "MDML"), R = as.binmat(N.R),
                 P.K = rep(1/nstates, nstates),
                 beta = rep(0.1, nitems), eta = rep(0.1, nitems),
                 betafix = rep(NA, nitems), etafix = rep(NA, nitems),
                 betaequal = NULL, etaequal = NULL,
                 randinit = FALSE, incradius = 0,
                 tol = 1e-07, maxiter = 10000, zeropad = 16) {

  K       <- as.matrix(K)
  N.R     <- setNames(as.integer(N.R), names(N.R))  # convert to named int
  N       <- sum(N.R)
  nitems  <- ncol(K)
  npat    <- nrow(R)
  nstates <- nrow(K)

  ## Uniformly random initial values
  if (randinit) {
      beta <- runif(nitems)                       # constraint: beta + eta < 1
       eta <- runif(nitems)
      beta <- ifelse(beta + eta < 1, beta, 1 - beta)
       eta <- ifelse(beta + eta < 1,  eta, 1 -  eta)
         x <- c(0, sort(runif(nstates - 1)), 1)
       P.K <- x[-1] - x[-length(x)]               # constraint: sum(P.K) == 1
  }

  ## Parameter restrictions
  betaeq <- etaeq <- diag(nitems)
  if (!is.null(betaequal)) for (i in betaequal) betaeq[i, i] <- 1
  if (!is.null( etaequal)) for (i in  etaequal)  etaeq[i, i] <- 1
  beta[!is.na(betafix)] <- betafix[!is.na(betafix)]    # overrides arguments
   eta[!is.na( etafix)] <-  etafix[!is.na( etafix)]

  names(P.K) <- if(is.null(rownames(K))) as.pattern(K) else rownames(K)
  names(beta) <- names(eta) <-
    if (is.null(colnames(K))) {
      make.unique(c("a", letters[(seq_len(nitems) %% 26) + 1])[-(nitems + 1)],
                  sep = "")
    } else colnames(K)
  dimnames(betaeq) <- dimnames(etaeq) <- list(names(eta), names(eta))

  ## Assigning state K given response R
  if(length(which(c(betafix, etafix) == 0))) {
    d.RK <- apply(K, 1, function(k) {
      RwoK <- t(R) & !k
      idx <- which(RwoK, arr.ind=TRUE)
      RwoK[idx[idx[, "row"] %in% which(etafix == 0), ]] <- NA
      
      KwoR <- k & !t(R)
      idx <- which(KwoR, arr.ind=TRUE)
      KwoR[idx[idx[, "row"] %in% which(betafix == 0), ]] <- NA
      colSums(RwoK) + colSums(KwoR)
    })
    PRKfun <- getPRK[["apply"]] 
  } else {
    d.RK <- apply(K, 1, function(k) colSums(xor(t(R), k)))
    PRKfun <- getPRK[["matmult"]] 
  }
  d.min <- apply(d.RK, 1, min, na.rm = TRUE)             # minimum discrepancy
  i.RK  <- (d.RK <= (d.min + incradius)) & !is.na(d.RK)

  ## Minimum discrepancy distribution
  disc.tab <- xtabs(N.R ~ d.min)
  disc     <- as.numeric(names(disc.tab)) %*% disc.tab / N

  ## Call EM
  method <- match.arg(method)
  opt <- blimEM(P.K = P.K, beta = beta, eta = eta, K = K, R = R, N.R = N.R,
                N = N, nitems = nitems, i.RK = i.RK, PRKfun = PRKfun,
                betafix = betafix, etafix = etafix, betaeq = betaeq,
                etaeq = etaeq, method = method, tol = tol, maxiter = maxiter)
  P.K  <- opt$P.K
  beta <- opt$beta
  eta  <- opt$eta
  iter <- opt$iter

  ## Mean number of errors
  P.Kq <- numeric(nitems)
  for(j in seq_len(nitems))
    P.Kq[j] <- sum(P.K[K[, j] == 1])
  nerror <- c("careless error" = sum(beta * P.Kq),
                 "lucky guess" = sum( eta * (1 - P.Kq)))

  ## If there are missing response patterns, create complete R and N.R
  if(npat < 2^nitems && nitems <= zeropad) {
    N.Rincomp <- N.R
    R   <- expand.grid(rep(list(0:1), nitems), KEEP.OUT.ATTRS=FALSE)
    N.R <- setNames(integer(nrow(R)), as.pattern(R)) # named int filled w/zeros
    R   <- as.binmat(N.R)                            # named int again
    N.R[names(N.Rincomp)] <- N.Rincomp
  }

  ## Recompute predictions and likelihood
  P.R.K <- do.call(PRKfun, list(beta, eta, K, R))
  P.R <- as.numeric(P.R.K %*% P.K)
  if (sum(P.R) < 1) P.R <- P.R/sum(P.R)      # if no zero padding: normalize
  loglik <- sum(log(P.R) * N.R, na.rm=TRUE)

  ## Number of parameters
  npar <- nstates - 1 + qr(betaeq)$rank - sum(!is.na(betafix)) +
                        qr( etaeq)$rank - sum(!is.na( etafix))

  ## Goodness of fit, df = number of patterns or persons
  fitted <- setNames(N*P.R, names(N.R))
  G2     <- 2*sum(N.R*log(N.R/fitted), na.rm=TRUE)
# df     <- min(2^nitems - 1, N) - npar        # number of patterns or persons
  df     <- min(if(nitems <= zeropad) 2^nitems - 1 else npat, N) - npar
  gof    <- c(G2=G2, df=df, pval = 1 - pchisq(G2, df))

  z <- list(discrepancy=c(disc), P.K=P.K, beta=beta, eta=eta,
    disc.tab=disc.tab, K=K, N.R=N.R, nitems=nitems, nstates=nstates,
    npatterns=npat, ntotal=N, nerror=nerror, npar=npar,
    method=method, iter=iter, loglik=loglik, fitted.values=fitted,
    goodness.of.fit=gof)
  class(z) <- "blim"
  z
}


## EM algorithm
blimEM <- function(P.K, beta, eta, K, R, N.R, N, nitems, i.RK, PRKfun,
                   betafix, etafix, betaeq, etaeq, method, tol, maxiter){

  eps     <- 1e-06
  iter    <- 0
  maxdiff <- 2 * tol
  em      <- c(MD = 0, ML = 1, MDML = 1)[method]
  md      <- c(MD = 1, ML = 0, MDML = 1)[method]
  beta.num <- beta.denom <- eta.num <- eta.denom <- beta
  while((maxdiff > tol) && (iter < maxiter) &&
        ((md*(1 - em) != 1) || (iter == 0))) {
    pi.old   <- P.K
    beta.old <- beta
    eta.old  <- eta

    P.R.K <- do.call(PRKfun, list(beta, eta, K, R))  # P(R|K)
    P.R   <- as.numeric(P.R.K %*% P.K)
    P.K.R <- P.R.K * outer(1/P.R, P.K)         # prediction of P(K|R)
    m.RK  <- i.RK^md * P.K.R^em
    m.RK  <- (m.RK / rowSums(m.RK)) * N.R      # m.RK = E(M.RK) = P(K|R)*N(R)

    ## Distribution of knowledge states
    P.K <- colSums(m.RK) / N

    ## Careless error and guessing parameters
    for (j in seq_len(nitems)) {
      beta.num[j]   <- sum(m.RK[R[, j] == 0, K[, j] == 1])
      beta.denom[j] <- sum(m.RK[           , K[, j] == 1])
       eta.num[j]   <- sum(m.RK[R[, j] == 1, K[, j] == 0])
       eta.denom[j] <- sum(m.RK[           , K[, j] == 0])
    }
    beta <- drop(betaeq %*% beta.num / betaeq %*% beta.denom)
     eta <- drop( etaeq %*%  eta.num /  etaeq %*%  eta.denom)
    beta[is.na(beta) | beta < eps] <- eps  # force 0 < beta, eta < 1
     eta[is.na( eta) |  eta < eps] <- eps
    beta[beta > 1 - eps] <- 1 - eps
     eta[ eta > 1 - eps] <- 1 - eps
    beta[!is.na(betafix)] <- betafix[!is.na(betafix)]  # reset fixed parameters
     eta[!is.na( etafix)] <-  etafix[!is.na( etafix)]

    maxdiff <- max(abs(c(P.K, beta, eta) - c(pi.old, beta.old, eta.old)))
    iter <- iter + 1
  }
  if(iter >= maxiter) warning("iteration maximum has been exceeded")
  out <- list(P.K = P.K, beta = beta, eta = eta, iter = iter)
  out
}


## Conditional distribution of response patterns given knowledge state P(R|K)
getPRK <- list(
  ## Slow algorithm
  apply = function(beta, eta, K, R)
    apply(K, 1,
          function(k) apply(
                    beta ^((1 - t(R)) *      k ) *
               (1 - beta)^(     t(R)  *      k ) *
                     eta ^(     t(R)  * (1 - k)) *
               (1 -  eta)^((1 - t(R)) * (1 - k)),
            2, prod)
    ),
  ## Vectorized algorithm, requires 0 < beta, eta < 1
  matmult = function(beta, eta, K, R)
    exp(
        (1 - R) %*% diag(log(    beta)) %*% t(    K) +
             R  %*% diag(log(1 - beta)) %*% t(    K) +
             R  %*% diag(log(     eta)) %*% t(1 - K) +
        (1 - R) %*% diag(log(1 -  eta)) %*% t(1 - K)
    )
)




#' Print a blim Object
#' 
#' Prints the output of a \code{blim} model object.
#' 
#' 
#' @param x an object of class \code{blim}, typically the result of a call to
#' \code{\link{blim}}.
#' @param P.Kshow logical, should the estimated distribution of knowledge
#' states be printed?
#' @param errshow logical, should the estimates of careless error and lucky
#' guess parameters be printed?
#' @param digits a non-null value for \code{digits} specifies the minimum
#' number of significant digits to be printed in values.
#' @param \dots further arguments passed to or from other methods.  None are
#' used in this method.
#' @return Returns the \code{blim} object invisibly.
#' @seealso \code{\link{blim}}.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#'  
#' blim1 <- blim(DoignonFalmagne7$K, DoignonFalmagne7$N.R)
#' print(blim1, showP.K = TRUE)
#' 
#' @export print.blim
print.blim <- function(x, P.Kshow = FALSE, errshow = TRUE,
  digits=max(3, getOption("digits") - 2), ...){
  cat("\nBasic local independence models (BLIMs)\n")
  cat("\nNumber of knowledge states:", x$nstates)
  cat("\nNumber of response patterns:", x$npatterns)
  cat("\nNumber of respondents:", x$ntotal)

  method <- switch(x$method,
            MD = "Minimum discrepancy",
            ML = "Maximum likelihood",
          MDML = "Minimum discrepancy maximum likelihood")
  cat("\n\nMethod:", method)
  cat("\nNumber of iterations:", x$iter)
  G2   <- x$goodness.of.fit[1]
  df   <- x$goodness.of.fit[2]
  pval <- x$goodness.of.fit[3]
  cat("\nGoodness of fit (2 log likelihood ratio):\n")
  cat("\tG2(", df, ") = ", format(G2, digits=digits), ", p = ",
      format(pval, digits=digits), "\n", sep="")

  cat("\nMinimum discrepancy distribution (mean = ",
    round(x$discrepancy, digits=digits), ")\n", sep="")
  disc.tab <- x$disc.tab
  names(dimnames(disc.tab)) <- NULL
  print(disc.tab)
  cat("\nMean number of errors (total = ",
    round(sum(x$nerror), digits=digits), ")\n", sep="")
  print(x$nerror)
  if(P.Kshow){
    cat("\nDistribution of knowledge states\n")
    printCoefmat(cbind("P(K)"=x$P.K), digits=digits, cs.ind=1, tst.ind=NULL,
      zap.ind=1)
  }
  if(errshow){
    cat("\nError and guessing parameters\n")
    printCoefmat(cbind(beta=x$beta, eta=x$eta), digits=digits, cs.ind=1:2,
      tst.ind=NULL, zap.ind=1:2)
  }
  cat("\n")
  invisible(x)
}


## Log-likelihood for blim objects
#' @export
#' @method logLik blim
logLik.blim <- function(object, ...){
  if(length(list(...)))
    warning("extra arguments discarded")
  p <- object$npar
  val <- object$loglik
  attr(val, "df") <- p
  attr(val, "nobs") <- object$npatterns
  class(val) <- "logLik"
  val
}


## Number of observations
#' @export
#' @method nobs blim
nobs.blim <- function(object, ...) object$npatterns


## Residuals for BLIMs


#' Residuals for Basic Local Independence Models
#' 
#' Computes deviance and Pearson residuals for \code{blim} objects.
#' 
#' See \code{\link{residuals.glm}} for details.
#' 
#' @param object an object of class \code{blim}, typically the result of a call
#' to \code{\link{blim}}.
#' @param type the type of residuals which should be returned; the alternatives
#' are: \code{"deviance"} (default) and \code{"pearson"}.
#' @param \dots further arguments passed to or from other methods.  None are
#' used in this method.
#' @return A named vector of residuals having as many elements as response
#' patterns.
#' @seealso \code{\link{blim}}, \code{\link{residuals.glm}},
#' \code{\link{plot.blim}}.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#' blim1 <- blim(DoignonFalmagne7$K, DoignonFalmagne7$N.R)
#' 
#' sum( resid(blim1)^2 )                # likelihood ratio G2
#' sum( resid(blim1, "pearson")^2 )     # Pearson X2
#' 
#' @export residuals.blim
residuals.blim <- function(object, type=c("deviance", "pearson"), ...){

  dev.resids <- function(y, mu, wt)
    2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))

  type <- match.arg(type)
  wts <- object$ntotal
  y <- object$N.R / wts
  mu <- object$fitted.values/wts
  res <- switch(type,
    deviance = if(object$goodness['df'] > 0){
        d.res <- sqrt(pmax(dev.resids(y, mu, wts), 0))
        ifelse(y > mu, d.res, -d.res)  # sign
      }
      else rep.int(0, length(mu)),
    pearson = (y - mu) * sqrt(wts)/sqrt(mu)
  )
  if(!is.null(object$na.action)) res <- naresid(object$na.action, res)
  res
}


## Diagnostic plot for BLIMs


#' Diagnostic Plot for Basic Local Independence Models
#' 
#' Plots BLIM residuals against fitted values.
#' 
#' The deviance residuals are plotted against the predicted response
#' probabilities for each response pattern.
#' 
#' @param x an object of class \code{blim}, typically the result of a call to
#' \code{\link{blim}}.
#' @param xlab,ylab, graphical parameters passed to plot.
#' @param list() graphical parameters passed to plot.
#' @seealso \code{\link{blim}}, \code{\link{residuals.blim}}.
#' @keywords models
#' @examples
#' 
#' ## Compare MD and MDML estimation
#' 
#' data(DoignonFalmagne7)
#' blim1 <- blim(DoignonFalmagne7$K, DoignonFalmagne7$N.R, method="MD")
#' blim2 <- blim(DoignonFalmagne7$K, DoignonFalmagne7$N.R, method="MDML")
#' 
#' par(mfrow = 1:2)      # residuals versus fitted values
#' plot(blim1, main = "MD estimation",   ylim = c(-4, 4))
#' plot(blim2, main = "MDML estimation", ylim = c(-4, 4))
#' 
#' @export plot.blim
plot.blim <- function(x,
  xlab="Predicted response probabilities", ylab="Deviance residuals", ...){

  xres <- resid(x)
  mu   <- x$fitted.values/x$ntotal
  plot(mu, xres, xlab = xlab, ylab = ylab, type="n", ...)
  abline(h = 0, lty = 2)
  panel.smooth(mu, xres)
}


## Simulate responses from BLIM


#' Simulate Responses from Basic Local Independence Models (BLIMs)
#' 
#' Simulates responses from the distribution corresponding to a fitted
#' \code{blim} model object.
#' 
#' Responses are simulated in two steps: First, a knowledge state is drawn with
#' probability \code{P.K}.  Second, responses are generated by applying
#' \code{\link{rbinom}} with probabilities computed from the model object's
#' \code{beta} and \code{eta} components.
#' 
#' @param object an object of class \code{blim}, typically the result of a call
#' to \code{\link{blim}}.
#' @param nsim currently not used.
#' @param seed currently not used.
#' @param \dots further arguments passed to or from other methods.  None are
#' used in this method.
#' @return A named vector of frequencies of response patterns.
#' @seealso \code{\link{blim}}, \code{\link{endm}}.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#'  
#' m1 <- blim(DoignonFalmagne7$K, DoignonFalmagne7$N.R)
#' simulate(m1)
#' 
#' ## Parametric bootstrap for the BLIM
#' disc <- replicate(200, blim(m1$K, simulate(m1))$discrepancy)
#' 
#' hist(disc, col = "lightgray", border = "white", freq = FALSE, breaks = 20,
#'      main = "BLIM parametric bootstrap", xlim = c(.05, .3))
#' abline(v = m1$discrepancy, lty = 2)
#' 
#' ## Parameter recovery for the SLM
#' m0 <- list( P.K = getSlmPK( g = rep(.8, 5),
#'                             K = DoignonFalmagne7$K,
#'                            Ko = getKFringe(DoignonFalmagne7$K)),
#'            beta = rep(.1, 5),
#'             eta = rep(.1, 5),
#'               K = DoignonFalmagne7$K,
#'          ntotal = 800)
#' class(m0) <- c("slm", "blim")
#' 
#' pars <- replicate(20, coef(slm(m0$K, simulate(m0), method = "ML")))
#' boxplot(t(pars), horizontal = TRUE, las = 1,
#'         main = "SLM parameter recovery")
#' 
#' ## See ?endm for further examples.
#' 
#' @export simulate.blim
simulate.blim <- function(object, nsim = 1, seed = NULL, ...){
     P.K <- object$P.K
    beta <- object$beta
     eta <- object$eta
      tK <- t(as.matrix(object$K))
       N <- object$ntotal
  nitems <- nrow(tK)

  state.id <- sample(seq_along(P.K), N, replace=TRUE, prob=P.K)  # draw states

  P.1.K <- tK*(1 - beta) + (1 - tK)*eta               # P(resp = 1 | K)
  R     <- matrix(0, N, nitems)                       # response matrix
  for(i in seq_len(N))
    R[i,] <- rbinom(nitems, 1, P.1.K[, state.id[i]])  # draw a response

  as.pattern(R, freq = TRUE)
}

##
#' @export
#' @method anova blim
anova.blim <- function(object, ..., test = c("Chisq", "none")){
  ## Adapted form MASS::anova.polr and stats::anova.glmlist

  test <- match.arg(test)
  dots <- list(...)
  if (length(dots) == 0)
      stop('anova is not implemented for a single "blim" object')
  mlist <- list(object, ...)
  nmodels <- length(mlist)
  names(mlist) <- sapply(match.call()[-1],
      function(s) paste(deparse(s), collapse="\n"))[seq_len(nmodels)]

  if (any(!sapply(mlist, inherits, "blim")))
      stop('not all objects are of class "blim"')

  ns <- sapply(mlist, function(x) length(x$fitted))
  if (any(ns != ns[1]))
      stop("models were not all fitted to the same size of dataset")

  dfs <- sapply(mlist, function(x) x$goodness.of.fit["df"])
  lls <- sapply(mlist, function(x) x$goodness.of.fit["G2"])
  df <- c(NA, -diff(dfs))
  x2 <- c(NA, -diff(lls))
  pr <- c(NA, pchisq(x2[-1], df[-1], lower.tail = FALSE))

  out <- data.frame(Resid.df = dfs, Deviance = lls, Df = df, Chisq = x2,
                    Prob = pr)
  dimnames(out) <- list(1:nmodels, c("Resid. Df", "Resid. Dev", "Df",
                                     "Deviance", "Pr(>Chi)"))
  if (test == "none") out <- out[, -ncol(out)]

  structure(out,
            heading = c("Analysis of Deviance Table\n",
                        paste0("Model ", format(1L:nmodels), ": ",
                               names(mlist), collapse = "\n")),
            class = c("anova", "data.frame"))
}

##
#' @export
#' @method deviance blim
deviance.blim <- function(object, ...) object$goodness.of.fit["G2"]

##
#' @export
#' @method coef blim
coef.blim <- function(object, ...){
  c(setNames(object$beta, paste("beta", names(object$beta), sep=".")),
    setNames(object$eta,  paste( "eta", names(object$eta),  sep=".")),
    object$P.K)
}

