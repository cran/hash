\name{set-methods}
\docType{methods}
\alias{set-methods}
\alias{set,data.frame,hash-method}
\alias{set,environment,hash-method}
\alias{set,hash,hash-method}
\alias{set,list,hash-method}
\alias{set,vector,vector-method}
\title{ Assigns a key-value pair(s) to a hash }

\description{

\code{set} is a method for assigning values to an object.  The arguments to 
set should always be names, values, location ... in that orger.

\code{set} should make smart guess based on its arguments and do the  
expected thing.  See the Methods section for a list of what this is.

}

\section{Methods}{
\describe{

    \item{x = "data.frame", y = "hash"}{ 
        Set keys to the first column, values to the second column of a data.frame.
    }

    \item{x = "environment", y = "hash"}{ 
        Set key-values pair(s) from an existing environment.    
    }

    \item{x = "hash", y = "hash"}{ 
        Set key-value pair(s) from an existing hash. 
    }

    \item{x = "list", y = "hash"}{ 
        Set key-value pair(s) from an existing list 
    }

    \item{x = "vector", y = "vector"}{ 
        Set key-value pair(s) from two vectors.  The first is taken as keys. 
        The second is taken as values.
    }
}
}


\value{
  \code{set} exists solely for its side-effects. No values are returned.
}

\author{ Christopher Brown }
\note{ 
}

\seealso{ See Also 
    \code{\link{set}}. 
    \code{\link{hash}}, 
    \code{\link{environment}} 
}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
    
