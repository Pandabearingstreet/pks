
\name{rename_duplicates}
\alias{rename_duplicates}
\title{Additional helper functions
Rename duplicate items in a skill function}
\usage{
rename_duplicates(skillfun, itemID = 1)
}
\arguments{
\item{skillfun}{a data frame representing a skill function}

\item{itemID}{index of the column in \code{skillfun} that holds the item indicator}
}
\value{
a data frame representing the skill function with renamed duplicate items
}
\description{
Adds a numeric prefix to duplicate items in a skill function
}
