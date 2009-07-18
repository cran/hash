\name{.get}

\alias{.get}

\title{ Get values from a hash }

\description{

    Returns the values of a hash. By default, it attempts to simplify the 
    results as a vector ( or matrixs ) if possible.  

    This funciton is hidden and should not normally be needed.  The accessor
    methods: \code{ \$, [ and [[ }  provide for much more convenient access.  

    See \code{\link{hash}}

}

\usage{
    .get(hash, keys, ... )
}

\arguments{
  \item{hash}{ A \code{\link{hash}} object. }
  \item{keys}{ A vector coerced to valid key names by \code{\link{validate.key}} }
  \item{...}{ Further arguments supplied to \code{\link{sapply}}. }
}

\details{

    Takes the keys, coerces them to valid keys and returns the associated 
    values in a form dictated by \code{...} arguments supllied to sapply 
    such as \code{simplify} or \code{USE.NAMES}.  See \code{\link{sapply}}
    for details.

}

\value{
  A list or vector of hash values in 
}

\author{ Christopher Brown }

\seealso{ 
  See Also \code{\link{hash}}, \code{\link{.set}}, \code{\link{sapply}} 
}


\examples{
  h <- hash( letters, 1:26 )
 .get( h, "b" )  # 2
}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
