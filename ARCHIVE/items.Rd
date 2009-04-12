\name{items}
\alias{items}
\alias{as.list}
\title{ return a hash as a named list }
\description{
    \code{items} returns a \code{\link{hash}} object as a list with namce.
}
\usage{
items(x)
}
\arguments{
  \item{x}{  A \code{\link{hash}} object. }
}
\details{
  \code{items} simply coerces the hash object to a list.  List items are
  given the name of the keys.
}
\value{
  \item{list}{A list with items names after the hash keys.}
}
\author{ Christopher Brown }
\seealso{ See also \code{\link{hash}}, \code{\link{get}}. }

\examples{
    h <- hash( letters, 1:26 )
    h.list <- items( h )
    length(h.list) == 26
}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
