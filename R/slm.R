## Fitting the simple learning model (SLM) by MDML


#' Simple Learning Models (SLMs)
#' 
#' Fits a simple learning model (SLM) for probabilistic knowledge structures by
#' minimum discrepancy maximum likelihood estimation.
#' 
#' See Doignon and Falmagne (1999) for details on the simple learning model
#' (SLM) for probabilistic knowledge structures. The model requires a
#' well-graded knowledge space \code{K}.
#' 
#' An \code{slm} object inherits from class \code{blim}.  See \code{blim} for
#' details on the function arguments.  The helper function \code{getSlmPK}
#' returns the distribution of knowledge states \code{P.K}.
#' 
#' @aliases slm getSlmPK coef.slm print.slm
#' @param K a state-by-problem indicator matrix representing the knowledge
#' space.  An element is one if the problem is contained in the state, and else
#' zero.
#' @param N.R a (named) vector of absolute frequencies of response patterns.
#' @param method \code{MD} for minimum discrepancy estimation, \code{ML} for
#' maximum likelihood estimation, \code{MDML} for minimum discrepancy maximum
#' likelihood estimation.
#' @param R a person-by-problem indicator matrix of unique response patterns.
#' Per default inferred from the names of \code{N.R}.
#' @param beta,eta,g vectors of initial values for the error, guessing, and
#' solvability parameters.
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
#' @param checkK logical, if \code{TRUE} K is checked for well-gradedness.
#' @param Ko a state-by-problem indicator matrix representing the outer fringe
#' for each knowledge state in \code{K}; typically the result of a call to
#' \code{getKFringe}.
#' @param x an object of class \code{slm}, typically the result of a call to
#' \code{slm}.
#' @param P.Kshow logical, should the estimated distribution of knowledge
#' states be printed?
#' @param parshow logical, should the estimates of error, guessing, and
#' solvability parameters be printed?
#' @param digits a non-null value for \code{digits} specifies the minimum
#' number of significant digits to be printed in values.
#' @param \dots additional arguments passed to other methods.
#' @return An object of class \code{slm} and \code{blim}.  It contains all
#' components of a \code{blim} object.  In addition, it includes: \item{g}{the
#' vector of estimates of the solvability parameters.}
#' @seealso \code{\link{blim}}, \code{\link{simulate.blim}},
#' \code{\link{getKFringe}}, \code{\link{is.downgradable}}
#' @references Doignon, J.-P., & Falmagne, J.-C. (1999).  \emph{Knowledge
#' spaces}. Berlin: Springer.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#' K   <- DoignonFalmagne7$K     # well-graded knowledge space
#' N.R <- DoignonFalmagne7$N.R   # frequencies of response patterns
#' 
#' ## Fit simple learning model (SLM) by different methods
#' slm(K, N.R, method = "MD")    # minimum discrepancy estimation
#' slm(K, N.R, method = "ML")    # maximum likelihood estimation by EM
#' slm(K, N.R, method = "MDML")  # MDML estimation
#' 
#' ## Compare SLM and BLIM
#' m1 <-  slm(K, N.R, method = "ML")
#' m2 <- blim(K, N.R, method = "ML")
#' anova(m1, m2)
#' 
#' @export slm
slm <- function(K, N.R, method = c("MD", "ML", "MDML"), R = as.binmat(N.R),
                beta = rep(0.1, nitems), eta = rep(0.1, nitems),
                g = rep(0.1, nitems),
                betafix = rep(NA, nitems), etafix = rep(NA, nitems),
                betaequal = NULL, etaequal = NULL,
                randinit = FALSE, incradius = 0,
                tol = 1e-07, maxiter = 10000, zeropad = 16,
                checkK = TRUE) {

  K       <- as.matrix(K)
  N.R     <- setNames(as.integer(N.R), names(N.R))  # convert to named int
  N       <- sum(N.R)
  nitems  <- ncol(K)
  npat    <- nrow(R)
  nstates <- nrow(K)

  # Doignon & Falmagne (2015, p. 8)
  # Def. 5: "A downgradable, finite knowledge space is called an antimatroid."
  # Theorem 7: An antimatroid is a well-graded knowledge space.
  if(checkK) stopifnot(is.knowledgespace(K), is.downgradable(K))

  Ko <- getKFringe(K, nstates, nitems)  # matrix of outer-fringe states

  ## Uniformly random initial values
  if (randinit) {
      beta <- runif(nitems)                       # constraint: beta + eta < 1
       eta <- runif(nitems)
      beta <- ifelse(beta + eta < 1, beta, 1 - beta)
       eta <- ifelse(beta + eta < 1,  eta, 1 -  eta)
         g <- runif(nitems)
  }

  ## Parameter restrictions
  betaeq <- etaeq <- diag(nitems)
  if (!is.null(betaequal)) for (i in betaequal) betaeq[i, i] <- 1
  if (!is.null( etaequal)) for (i in  etaequal)  etaeq[i, i] <- 1
  beta[!is.na(betafix)] <- betafix[!is.na(betafix)]    # overrides arguments
   eta[!is.na( etafix)] <-  etafix[!is.na( etafix)]

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
  opt <- slmEM(beta = beta, eta = eta, g = g, K = K, Ko = Ko, R = R,
               N.R = N.R, N = N, nitems = nitems, i.RK = i.RK,
               PRKfun = PRKfun,
               betafix = betafix, etafix = etafix, betaeq = betaeq,
               etaeq = etaeq, method = method, tol = tol, maxiter = maxiter)
  beta <- opt$beta
  eta  <- opt$eta
  g    <- opt$g
  iter <- opt$iter

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
  P.K <- getSlmPK(g, K, Ko)
  names(P.K) <- if(is.null(rownames(K))) as.pattern(K) else rownames(K)
  if(!isTRUE(all.equal(sum(P.K), 1)))
    warning("State probabilities P(K) do not sum to unity")
  P.R <- as.numeric(P.R.K %*% P.K)
  if (sum(P.R) < 1) P.R <- P.R/sum(P.R)      # if no zero padding: normalize
  loglik <- sum(log(P.R) * N.R, na.rm=TRUE)

  ## Mean number of errors
  P.Kq <- numeric(nitems)
  for(j in seq_len(nitems))
    P.Kq[j] <- sum(P.K[K[, j] == 1])
  nerror <- c("careless error" = sum(beta * P.Kq),
                 "lucky guess" = sum( eta * (1 - P.Kq)))

  ## Number of parameters
  npar <- qr(betaeq)$rank - sum(!is.na(betafix)) +
          qr( etaeq)$rank - sum(!is.na( etafix)) +
          nitems

  ## Goodness of fit, df = number of patterns or persons
  fitted <- setNames(N*P.R, names(N.R))
  G2     <- 2*sum(N.R*log(N.R/fitted), na.rm=TRUE)
# df     <- min(2^nitems - 1, N) - npar        # number of patterns or persons
  df     <- min(if(nitems <= zeropad) 2^nitems - 1 else npat, N) - npar
  gof    <- c(G2=G2, df=df, pval = 1 - pchisq(G2, df))

  z <- list(discrepancy=c(disc), P.K=P.K, beta=beta, eta=eta, g=g,
    disc.tab=disc.tab, K=K, N.R=N.R, nitems=nitems, nstates=nstates,
    npatterns=npat, ntotal=N, nerror=nerror, npar=npar,
    method=method, iter=iter, loglik=loglik, fitted.values=fitted,
    goodness.of.fit=gof)
  class(z) <- c("slm", "blim")
  z
}


## EM algorithm
slmEM <- function(beta, eta, g, K, Ko, R, N.R, N, nitems, i.RK, PRKfun,
                  betafix, etafix, betaeq, etaeq, method, tol, maxiter){

  eps     <- 1e-06
  iter    <- 0
  maxdiff <- 2 * tol
  em      <- c(MD = 0, ML = 1, MDML = 1)[method]
  md      <- c(MD = 1, ML = 0, MDML = 1)[method]
  beta.num <- beta.denom <- eta.num <- eta.denom <- g.o <- beta
  while((maxdiff > tol) && (iter < maxiter) &&
        ((md*(1 - em) != 1) || (iter == 0))) {
    beta.old <- beta
    eta.old  <- eta
    g.old    <- g

    P.R.K <- do.call(PRKfun, list(beta, eta, K, R))  # P(R|K)
    P.K   <- getSlmPK(g, K, Ko)
    P.R   <- as.numeric(P.R.K %*% P.K)
    P.K.R <- P.R.K * outer(1/P.R, P.K)         # prediction of P(K|R)
    m.RK  <- i.RK^md * P.K.R^em
    m.RK  <- (m.RK / rowSums(m.RK)) * N.R      # m.RK = E(M.RK) = P(K|R)*N(R)

    ## Careless error, guessing, and solvability parameters
    for(j in seq_len(nitems)) {
      beta.num[j]   <- sum(m.RK[R[, j] == 0,  K[, j] == 1])
      beta.denom[j] <- sum(m.RK[           ,  K[, j] == 1])
       eta.num[j]   <- sum(m.RK[R[, j] == 1,  K[, j] == 0])
       eta.denom[j] <- sum(m.RK[           ,  K[, j] == 0])
         g.o[j]     <- sum(m.RK[           , Ko[, j] == 1])
    }
    beta <- drop(betaeq %*% beta.num / betaeq %*% beta.denom)
     eta <- drop( etaeq %*%  eta.num /  etaeq %*%  eta.denom)
    beta[is.na(beta) | beta < eps] <- eps  # force 0 < beta, eta, g < 1
     eta[is.na( eta) |  eta < eps] <- eps
    beta[beta > 1 - eps] <- 1 - eps
     eta[ eta > 1 - eps] <- 1 - eps
    beta[!is.na(betafix)] <- betafix[!is.na(betafix)]  # reset fixed parameters
     eta[!is.na( etafix)] <-  etafix[!is.na( etafix)]
       g <- beta.denom / (beta.denom + g.o)
       g[is.na(g) | g < eps] <- eps
       g[g > 1 - eps] <- 1 - eps

    maxdiff <- max(abs(c(beta, eta, g) - c(beta.old, eta.old, g.old)))
    iter <- iter + 1
  }
  if(iter >= maxiter) warning("iteration maximum has been exceeded")
  out <- list(beta = beta, eta = eta, g = g, iter = iter)
  out
}


## Testing for closure under union
is.knowledgespace <- function(K) {
  all(
      sort(as.pattern(binary_closure(K == TRUE) + 0)) ==
      sort(as.pattern(K))
  )
}


## Obtain outer/inner fringe for each state in K


#' Outer and Inner Fringes of a Knowledge Structure
#' 
#' Returns the outer or inner fringe for each state in a knowledge structure.
#' 
#' The outer fringe of a knowledge state is the set of all items that can be
#' learned from that state, such that adding an outer-fringe item to the state
#' results in another state in \code{K},
#' 
#' \deqn{K^O = \{q \notin K | K \cup \{q\} \in \mathcal{K}\}.}
#' 
#' The inner fringe of a knowledge state is the set of all items that have been
#' learned most recently to reach that state, such that deleting an
#' inner-fringe item from the state results in another state in \code{K},
#' 
#' \deqn{K^I = \{q \in K | K - \{q\} \in \mathcal{K}\}.}
#' 
#' @param K a state-by-problem indicator matrix representing the knowledge
#' structure.  An element is one if the problem is contained in the state, and
#' else zero.
#' @param nstates the number of knowledge states in \code{K}.
#' @param nitems the number of items in \code{K}.
#' @param outer logical. If \code{TRUE} return outer fringe, else return inner
#' fringe.
#' @return A state-by-problem indicator matrix representing the outer or inner
#' fringe for each knowledge state in \code{K}.
#' @seealso \code{\link{slm}}, \code{\link{simulate.blim}}.
#' @keywords models
#' @examples
#' 
#' data(DoignonFalmagne7)
#' 
#' ## Which items can be learned from each state?
#' getKFringe(DoignonFalmagne7$K)
#' 
#' ## Which items in each state have been recently learned?
#' getKFringe(DoignonFalmagne7$K, outer = FALSE)
#' 
#' @export getKFringe
getKFringe <- function(K, nstates = nrow(K), nitems = ncol(K),
                       outer = TRUE) {
  stopifnot(
     is.numeric(K),
     is.matrix(K)
  )

  ## List of matrices containing the K' states with |K'| = |K| +/- 1
  add1 <- if(outer) 1 else -1
  Kadd1 <- vector(mode = "list", length = nstates)
  nItemsPerK <- rowSums(K)
  for(i in seq_len(nstates)) {
    Kadd1[[i]] <- K[nItemsPerK == nItemsPerK[i] + add1, , drop = FALSE]
  }

  getFringeItems <- function(k, kadd1States) {
    x <- xor(k, t(kadd1States))             # symm set diff with the K' states
    rowSums(x[, colSums(x) == 1, drop = FALSE])  # keep single-element diffs
  }
  fringe <- t(mapply(FUN = getFringeItems,
                     split(K, seq_len(nstates)),
                     Kadd1))
  rownames(fringe) <- rownames(K)
  mode(fringe) <- "integer"
  fringe
}


## Compute P(K) from g parameters
getSlmPK <- function(g, K, Ko) {
  # foreach k in K:
  #   prod(g[q-in-K]) * prod(1 - g[q-in-Ks-Ofringe])
  apply(K  == TRUE, 1, function(k) prod(g[k])) *    # use K as index
  apply(Ko == TRUE, 1, function(k) prod(1 - g[k]))
}

##
#' @export
#' @method print slm
print.slm <- function(x, P.Kshow = FALSE, parshow = TRUE,
  digits=max(3, getOption("digits") - 2), ...){
  cat("\nSimple learning models (SLMs)\n")
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
  if(parshow){
    cat("\nError, guessing, and solvability parameters\n")
    printCoefmat(cbind(beta=x$beta, eta=x$eta, g=x$g), digits=digits,
                 cs.ind=1:2, tst.ind=NULL, zap.ind=1:2)
  }
  cat("\n")
  invisible(x)
}

##
#' @export
#' @method coef slm
coef.slm <- function(object, ...){
  c(setNames(object$beta, paste("beta", names(object$beta), sep=".")),
    setNames(object$eta,  paste( "eta", names(object$eta),  sep=".")),
    setNames(object$g,    paste(   "g", names(object$g),    sep=".")))
}

