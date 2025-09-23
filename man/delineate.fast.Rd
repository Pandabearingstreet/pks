\name{delineate.fast}
\alias{delineate.fast}
\title{Delineate a Knowledge Structure by a Skill Function (Alternative Implementation)}
\usage{
delineate.fast(skillfun, itemID = 1, states_as_matrix=TRUE)
}
\arguments{
\item{skillfun}{a data frame representing the skill function.
It consists of an item indicator and a problem-by-skill indicator matrix.}

\item{itemID}{index of the column in \code{skillfun} that holds the item
indicator.}
\item{states_as_matrix}{logical; if FALSE, return only a list of states}
}
\value{
A list with the following components:
\item{K}{A matrix representing the knowledge structure delineated by the skill function (if \code{states_as_matrix = TRUE}).}
\item{intents}{A named list of matrices. Each name corresponds to a state, and each matrix contains the intents for that state, with columns preserved and entries as 0/1.}
\item{statenames}{A character vector of state names (if \code{states_as_matrix = FALSE}).}
\item{items}{A character vector of unique item names (if \code{states_as_matrix = FALSE}).}
}
\description{
Computes the knowledge structure delineated by a skill function using
specialized algorithms for conjunctive, disjunctive, and mixed skill functions.
}
\details{
This implementation automatically detects the type of skill function and applies
the appropriate delineation algorithm, which can be more efficient than the
general approach used by \code{\link{delineate}} for large skill functions.
For smaller skill functions, \code{\link{delineate}} is faster.

The \code{intents} component is now a named list, where each entry is a matrix with the same columns as the skill function, and entries are 0/1 (not logical). Each name corresponds to a state pattern.
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

 
K <- delineate.fast(sf)$K
# for larger skillfunctions, the full knowledge matrix can be quite large. 
# in this case, it is better to set states_as_matrix to FALSE, 
# to only return the state name and unique item names
output <- delineate.fast(sf, states_as_matrix = FALSE)

# these are of cause equivalent
statenames <- output$statenames
items <- output$items
K2 <- as.binmat(statenames, col.names = items)
K.are.same(K, K2)

## Accessing intents
intents_list <- delineate.fast(sf)$intents
str(intents_list) # named list of matrices

## Plotting the knowledge structure
if(requireNamespace("relations") &&
   requireNamespace("Rgraphviz")) {
  rownames(K) <- as.pattern(K, useNames = TRUE)
  plot(relations::as.relation(is.subset(K)), main = "")
}
}