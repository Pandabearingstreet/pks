#' Jacobian Matrix for Basic Local Independence Model
#' 
#' Computes the Jacobian matrix for a basic local independence model (BLIM).
#' 
#' This is a draft version.  It may change in future releases.
#' 
#' @param object an object of class \code{blim}, typically the result of a call
#' to \code{\link{blim}}.
#' @param P.K the vector of parameter values for probabilities of knowledge
#' states.
#' @param beta the vector of parameter values for probabilities of a careless
#' error.
#' @param eta the vector of parameter values for probabilities of a lucky
#' guess.
#' @param betafix,etafix vectors of fixed error and guessing parameter values;
#' \code{NA} indicates a free parameter.
#' @return The Jacobian matrix.  The number of rows equals 2^(number of items)
#' - 1, the number of columns equals the number of independent parameters in
#' the model.
#' @seealso \code{\link{blim}}, \code{\link{simulate.blim}},
#' \code{\link{gradedness}}.
#' @references Heller, J. (2017).  Identifiability in probabilistic knowledge
#' structures.  \emph{Journal of Mathematical Psychology}, \bold{77}, 46--57.
#' c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1016/j.jmp.2016.07.008")\Sexpr{tools:::Rd_expr_doi("10.1016/j.jmp.2016.07.008")}
#' 
#' Stefanutti, L., Heller, J., Anselmi, P., & Robusto, E. (2012).  Assessing
#' the local identifiability of probabilistic knowledge structures.
#' \emph{Behavior Research Methods}, \bold{44}(4), 1197--1211.
#' c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.3758/s13428-012-0187-z")\Sexpr{tools:::Rd_expr_doi("10.3758/s13428-012-0187-z")}
#' @keywords models
#' @examples
#' 
#' data(endm)
#' m <- blim(endm$K2, endm$N.R)
#' 
#' ## Test of identifiability
#' J <- jacobian(m)
#' dim(J)
#' qr(J)$rank
#' 
#' @export jacobian
jacobian <- function(object, P.K = rep(1/nstates, nstates),
                     beta = rep(0.1, nitems), eta = rep(0.1, nitems),
                     betafix = rep(NA, nitems), etafix = rep(NA, nitems))
{
  K <- as.matrix(object$K)
# N <- object$ntotal
  nitems <- ncol(K)
  nstates <- nrow(K)
  R <- as.matrix(expand.grid(rep(list(0:1), nitems)))
  rownames(R) <- as.pattern(R)
  npatterns <- nrow(R)

  PRKfun <- if(length(which(c(betafix, etafix) == 0))) {
    getPRK[["apply"]] 
  } else {
    getPRK[["matmult"]] 
  }
  betanew <- beta
   etanew <-  eta
  betanew[!is.na(betafix)] <- betafix[!is.na(betafix)]
   etanew[!is.na( etafix)] <-  etafix[!is.na( etafix)]
  P.R.K <- do.call(PRKfun, list(betanew, etanew, K, R))

# P.R <- as.numeric(P.R.K %*% P.K)
  K.star <- K[-nstates, ]
  R.star <- R[-npatterns, ]
  P.K.star <- P.K[-nstates]
  P.R.K.star <- P.R.K[-npatterns, -nstates]
  P <- P.R.K.star - outer(P.R.K[-npatterns, nstates], rep(1, nstates - 1))
  colnames(P) <- paste("pi", as.pattern(K.star), sep="_")
  B <- sapply(1:nitems, function(q) (as.matrix(P.R.K.star[, K.star[, q] == 1]) %*% P.K.star[K.star[, q] == 1] +
      P.R.K[-npatterns, nstates] * P.K[nstates]) * (-1/(1 - beta[q]))^R.star[, q] * (1/beta[q])^(1-R.star[, q]))
  colnames(B) <- paste("beta", colnames(K), sep="_")
  E <- sapply(1:nitems, function(q) (as.matrix(P.R.K.star[, K.star[, q] == 0]) %*% P.K.star[K.star[, q] == 0]) *
      (1/eta[q])^R.star[, q] * (-1/(1-eta[q]))^(1-R.star[, q]))
  colnames(E) <- paste("eta", colnames(K), sep="_")
  J <- cbind(P, B, E)
  rownames(J) <- rownames(R.star)
  J
}

