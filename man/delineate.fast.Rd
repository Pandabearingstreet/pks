\name{delineate.fast}
\alias{delineate.fast}
\title{Delineate a Knowledge Structure by a Skill Function (Alternative Implementation)}
\usage{
delineate.fast(skillfun, itemID = 1, give_full_matrix_result=TRUE)
}
\arguments{
\item{skillfun}{a data frame representing the skill function.
It consists of an item indicator and a problem-by-skill indicator matrix.}

\item{itemID}{index of the column in \code{skillfun} that holds the item
indicator.}
\item{give_full_matrix_result}{logical; if FALSE, return only a list of states}
}
\value{
A matrix representing the knowledge structure delineated by the skill function.
}
\description{
Computes the knowledge structure delineated by a skill function using
specialized algorithms for conjunctive, disjunctive, and mixed skill functions.
}
\details{
This implementation automatically detects the type of skill function and applies
the appropriate delineation algorithm, which can be more efficient than the
general approach used by \code{\link{delineate}} for large skill functions
for smaller skillfunctions, \code{\link{delineate}} is faster.
}
\seealso{
\code{\link{delineate}}
}

\examples{
# Skill function
# mu(e) = {{s, t}, {s, u}},  mu(f) = {{u}}
# mu(g) = {{s}, {t}},        mu(h) = {{t}}
sf <- read.table(header = TRUE, text = "
  item s t u
     e 1 1 0
     e 1 0 1
     f 0 0 1
     g 1 0 0
     g 0 1 0
     h 0 1 0
")

 
K <- delineate.fast(sf)
# for larger skillfunctions, the full knowledge matrix can be quite large. 
# in this case, it is better to set give_full_matrix_result to FALSE, 
# to only return the state name and unique item names
output <- delineate.fast(sf, give_full_matrix_result = FALSE)

# these are of cause equivalent
statenames <- output$statenames
items <- output$items
K2 <- as.binmat(statenames, col.names = items)
K.are.same(K, K2)

## Plotting the knowledge structure
if(requireNamespace("relations") &&
   requireNamespace("Rgraphviz")) {
  rownames(K) <- as.pattern(K, useNames = TRUE)
  plot(relations::as.relation(is.subset(K)), main = "")
}
}