\name{values}
\alias{values}
\alias{values-methods}
\alias{values,hash-method}
\title{ 
    Extract values of a hash object.    
}

\description{
    Extract \code{values} from a \code{hash} object.  This is a pseudo-
    accessor method that returns all of the values without requiring a 
    key.  It is identical to \code{ h[[ keys(h) ]] }.

    For details about hash accessors, please see \code{\link{hash-class}}

}

\usage{
values(x, ...)
}

\arguments{
  \item{x}{ The \code{\link{hash}} from where the values retrieved }
  \item{...}{ Arguments passed to \code{sapply} }
}

\details{

    The \code{values} method returns the values from a hash in the 
    simplest form available.  
    
    Values are extracted using sapply so if the values are coerced 
    to a vector or matrixs if possible and elements are named after 
    the corresponding key. If the values are of different types or 
    of a complex class than a named list is returned.  Supply 
    arguments to \code{sapply} can force this case.

}

\value{
Please see details for which value will be returned:

  \item{vector}{Vector with the type as the values of the hash}
  \item{list}{list containing the values of the hash}
  
}

\references{ none }
\author{ Christopher Brown }

\seealso{ See also \code{\link{hash}}, \code{\link{sapply}}. }

\examples{

    h <- hash( letters, 1:26 )
    values(h)  # 1:26
    values(h, simplify = FALSE )
    values(h, USE.NAMES = FALSE )

}        

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
