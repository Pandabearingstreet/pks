\name{generate.skillfun}
\alias{generate.skillfun}
\title{Generate a Random Skill Function}
\usage{
generate.skillfun(
  seed = 42,
  sparsity = 0.1,
  items = 6,
  skills = 12,
  conjunctive = FALSE,
  disjunctive = FALSE,
  lambda = 1
)
}
\arguments{
\item{seed}{random seed for reproducibility}

\item{sparsity}{probability of a skill being required for an item (between 0 and 1)}

\item{items}{number of items in the skill function}

\item{skills}{number of skills in the skill function}

\item{conjunctive}{logical; if TRUE, ensures each item label appears only once}

\item{disjunctive}{logical; if TRUE, ensures each item requires at most one skill}

\item{lambda}{lambda parameter for the Poisson distribution used to determine the number of alternative competencies per item. 
                      only apllies when \code{conjunctive} is FALSE and \code{disjunctive} is FALSE}
}
\value{
A data frame representing the skill function with columns for item labels
  and binary indicators for each skill
}
\description{
Creates a random skill function with specified properties for testing and
simulation purposes.
}
\details{
The skill function indicates which skills are required to solve each item.
This function can generate conjunctive, disjunctive, or mixed skill functions
with controlled sparsity. the items are numeric, to ensure scaleability beyond 26.
}
\examples{
# Generate a random conjunctive skill function with 4 items and 4 skills
sf <- generate.skillfun(items = 6, skills = 4)

K <- delineate(sf)$K
## Plotting the knowledge structure
if(requireNamespace("relations") &&
   requireNamespace("Rgraphviz")) {
  rownames(K) <- as.pattern(K, useNames = TRUE)
  plot(relations::as.relation(is.subset(K)), main = "")
}
}

