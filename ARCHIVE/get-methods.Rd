\name{get-methods}
\docType{methods}
\alias{get-methods}
\alias{get,ANY,ANY-method}
\alias{get,ANY,hash-method}
\title{ Returns the hash values of corresponding to the supplied keys }
\description{
    \code{get} returns the values associated with the supplied keys.  
    By default, it attempts to simplify the results using \code{sapply}.
}

\section{Methods}{
    \describe{

        \item{ x = "hash" }{
            return all values from a hash
        }
    
        \item{x = "ANY", pos = "hash"}{
            Coerces(x) to valid key names and then looks them up on the hash.
            Further arguments are passed to \code{sapply}.
        }

        \item{x = "ANY", pos = "ANY"}{ 
            generic function to allow dispatch to base::get  
        }

    }

}    

\author{ Christopher Brown }
\seealso{ See Also as \code{\link{hash}}, \code{\link{set}} }

\examples{
    h <- hash( letters, 1:26 )
    get( "b", h )  # 1
}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
