## Item tree analysis


#' Item Tree Analysis (ITA)
#' 
#' Item tree analysis (ITA) on a set of binary responses.
#' 
#' ITA seeks to establish a precedence relation among a set of binary items.
#' For each pair of items \eqn{(p, q)}, it counts how often \eqn{p} is not
#' solved if \eqn{q} is solved, which constitutes a violation of the relation.
#' ITA searches for a threshold \code{L} for the maximum number of violations
#' consistent with a (transitive) precedence relation.  Its attempts to
#' minimize the total discrepancy between \code{R} and \code{K}.
#' 
#' See van Leeuwe (1974) and Schrepp (1999) for details.
#' 
#' @aliases ita print.ita
#' @param R a subject-by-problem indicator matrix representing the responses.
#' @param L the threshold of violations acceptable for the precedence relation.
#' If \code{NULL} (default), an optimal threshold is searched for.
#' @param makeK should the corresponding knowledge structure be returned?
#' @param search local (default) or global threshold search.
#' @return An object of class \code{ita} having the following components:
#' \item{K}{the knowledge structure corresponding to the precedence relation.}
#' \item{discrepancy}{the discrepancy between \code{R} and \code{K} (fit),
#' between \code{K} and \code{R} (complexity), and their sum (total).}
#' \item{transitiveL}{the vector of transitive thresholds.}
#' \item{searchL}{either \code{NULL} or the method used for threshold search.}
#' \item{L}{the selected or requested threshold.} \item{P}{the precedence
#' matrix containing the number of violations.} \item{I}{the precedence
#' relation as a logical incidence matrix at threshold \code{L}.}
#' @seealso \code{\link{blim}}.
#' @references Schrepp, M. (1999).  On the empirical construction of
#' implications between bi-valued test items.  \emph{Mathematical Social
#' Sciences}, \bold{38}(3), 361--375.
#' c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1016/S0165-4896(99)00025-6")\Sexpr{tools:::Rd_expr_doi("10.1016/S0165-4896(99)00025-6")}
#' 
#' Van Leeuwe, J.F. (1974).  Item tree analysis.  \emph{Nederlands Tijdschrift
#' voor de Psychologie en haar Grensgebieden}, \bold{29}(6), 475--483.
#' @keywords models
#' @examples
#' 
#' data(chess)
#' 
#' ita(chess$R)  # find (locally) optimal threshold L
#' 
#' i <- ita(chess$R, L = 6, makeK = TRUE)
#' identical(sort(as.pattern(i$K)),
#'           sort(as.pattern(chess$dst1)))
#' 
#' ## Plotting the precedence relation
#' if(requireNamespace("relations") &&
#'    requireNamespace("Rgraphviz")) {
#'   plot(relations::as.relation(i$I))
#' }
#' 
#' @export ita
ita <- function(R, L = NULL, makeK = FALSE,
                search = c("local", "global")) {
  ## Helpers
  isTransitive <- function(x) {
    P <- bb <= x
    all(P[P %*% P > 0])
  }

  getKfromP <- function(x) rbind(0L, sets::binary_closure(t(x)))

  getdmin <- function(x) mean(apply(x, 1, min, na.rm = TRUE))

  getdisc <- function(R, K, total = TRUE) {
    d.RK <- apply(K, 1, function(k) colSums(xor(t(R), k)))
    disc <- sapply(list(fit = d.RK, complexity = t(d.RK)), getdmin)
    if(total) sum(disc) else disc
  }

  getoptimalL <- function(bb, R, transitiveL, search = search) {
    LL <- rev(transitiveL)
    if(length(LL) < 2)
      return(LL)
    P <- bb <= LL[1]          # start from largest threshold
    dsum1 <- getdisc(R, getKfromP(P))
    for(i in 2:length(LL)) {  # reduce threshold value
      P <- bb <= LL[i]
      dsum <- getdisc(R, getKfromP(P))
      if(search == "local" && dsum1 < dsum) {
        return(LL[i - 1])     # local: stop when dsum starts getting worse
      }
      if(dsum1 > dsum) {      # global: full search for optimal dsum
        dsum1 <- dsum
        optL <- LL[i]
      }
    }
    optL
  }

  stopifnot(is.matrix(R))
  nitems <- ncol(R)
  cn <- colnames(R)
  cn <- if(!is.null(cn)) cn else letters[seq_len(nitems)]

  ## Count all pairwise counterexamples to p < q
  i <- expand.grid(x = seq_len(nitems), y = seq_len(nitems))
  bb <- matrix(colSums((1 - R[, i$x]) * R[, i$y]),
               ncol = nitems, dimnames = list("<" = cn, ">" = cn))
  tL <- Filter(isTransitive, sort(unique(as.vector(bb))))

  ## Select optimal threshold value
  searchL <- NULL
  search <- match.arg(search)
  if(is.null(L)) {
    L <- getoptimalL(bb, R, transitiveL = tL, search = search)
    searchL <- search
  }

  ## Define relation according to threshold
  if(!isTransitive(L)) stop("relation not transitive for threshold L")
  P <- bb <= L

  ## Make K by closing its base (requires transitivity)
  K <- disc <- NULL
  if(makeK) {
    K <- getKfromP(P)
    rownames(K) <- as.pattern(K)
    disc <- getdisc(R, K, total = FALSE)
    disc <- c(disc, total = sum(disc))
  }
  retval <- list(K = K, discrepancy = disc, transitiveL = tL,
                 searchL = searchL, L = L, P = bb, I = P)
  class(retval) <- "ita"
  retval
}

##
#' @export
#' @method print ita
print.ita <- function(x, digits = max(3, getOption("digits") - 3),
                      ...){
  cat("\nItem tree analysis (ITA)\n")
  cat("\nViolations of precedence relation:\n")
  print(x$P)
  cat("\nSelected threshold (L):", x$L)
  cat("\nTransitive thresholds:\n")
  print(x$transitiveL)
  if(!is.null(x$K)) {
    cat("\nNumber of knowledge states:", nrow(x$K))
    cat("\nDiscrepancy:\n")
    print(x$discrepancy, digits = digits)
  }
  cat("\n")
  invisible(x)
}

