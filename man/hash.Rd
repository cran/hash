\name{hash}

\alias{hash}

\title{hash/associative array/dictionary data structure for the R language}

\description{

  Preferred constructor for the \code{\link{hash-class}}.
  
}

\usage{
hash(keys,values)
}

\arguments{
  \item{keys}{ Optional vector of keys  }
  \item{values}{ Optional argument of values }
}

\details{
  \code{hash} returns a hash object.  The first argument is taken as 
  the vector of keys and must be coercable to a character vector.  
  The second argument will be taken as the corresponding values.  See
  \code{\link{.set}} for how vectors of unequal length are interpretted
  which does most of the work of this function.
  
}

\value{
  An object of class hash.   
}

\author{ Christopher Brown }
\seealso{ \code{\link{.set}}, \code{\link{hash-accessors}} }

\examples{
    h <- hash()
    h <- hash( letters, 1:26 )

}
\keyword{ data }
\keyword{ manip }
