\name{validate.key}

\alias{validate.key}

\title{ creates/coerces objects to proper hash keys }

\description{
    Given an vector of any type, \code{validate.key} tries to coerce it into a
    character vector that can be used as a hash key.  This is used internally
    by the hash package and should not be normally needed.
}

\usage{
validate.key(key)
}

\arguments{
  \item{key}{ 
    An object that represents the key(s) to be coerced to a valid hash keys.
  }
}

\details{
    This function is used internally by the \code{\link{hash}} class to 
    ensure that the keys are valid. There should be no need to use this 
    externally and is only documented for completeness.
}

\value{
A character vector of valid keys                                             
}

\author{ Christopher Brown }

\seealso{ See also as \code{\link{hash}} }

\examples{

  validate.key( letters )
  validate.key( 1:26 ) 

}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
