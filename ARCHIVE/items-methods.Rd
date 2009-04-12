\name{items-methods}
\docType{methods}
\alias{items-methods}
\alias{items,hash-method}
\title{ return a hash as a named list }
\description{
    \code{items} returns a \code{\link{hash}} object as a list with namce.
}

\section{Methods}{
    \describe{

        \item{x = "hash"}{ 
            Coerces hash to list object with items named after the hash keys 
        }
    }
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
