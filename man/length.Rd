\name{length}

\alias{length}
\alias{length-methods}
\alias{length,hash-method}

\title{ Returns the number of items in a hash }

\description{
    Returns the number of items in a hash
}

\details{
    Return the number of items in the hash
}

\value{
  \item{integer}{ Number of items in the hash. }
}

\author{ Christpher Brown }

\seealso{ See Also \code{\link{hash}} }

\examples{

  h <- hash( letters, 1:26 )
  length(h) # 26

}

\keyword{ methods }
