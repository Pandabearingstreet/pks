
\name{disjunctive.as.conjunctive}
\alias{disjunctive.as.conjunctive}
\title{Convert a disjunctive skill function to a conjunctive one}
\usage{
disjunctive.as.conjunctive(sf)
}
\arguments{
\item{sf}{a data frame representing a disjunctive skill function}
}
\value{
a data frame representing the equivalent conjunctive skill function
}
\description{
Convert a disjunctive skill function to a conjunctive one
}
\examples{
# Skill function
# mu(e) = {{t}, {s}},  mu(f) = {{u}}
# mu(g) = {{s}, {t}},  mu(h) = {{t}}
sf <- read.table(header = TRUE, text = "
  item s t u
     e 0 1 0
     e 1 0 0
     f 0 0 1
     g 1 0 0
     g 0 1 0
     h 0 1 0
")
disjunctive.as.conjunctive(sf)
}
