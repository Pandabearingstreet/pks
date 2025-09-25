
\name{conjunctive.as.disjunctive}
\alias{conjunctive.as.disjunctive}
\title{Convert a conjunctive skill function to a disjunctive one}
\usage{
conjunctive.as.disjunctive(sf)
}
\arguments{
\item{sf}{a data frame representing a conjunctive skill function}
}
\value{
a data frame representing the equivalent disjunctive skill function
}
\description{
Convert a conjunctive skill function to a disjunctive one
}
\examples{
    # Skill function
    # mu(e) = {{t, s}}, mu(f) = {{u}}
    # mu(g) = {{s, t}}, mu(h) = {{t}}
    sf <- read.table(header = TRUE, text = "
      item s t u
         e 1 1 0
         f 0 0 1
         g 1 1 0
         h 0 1 0
    ")
    conjunctive.as.disjunctive(sf)
}
