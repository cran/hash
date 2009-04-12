\name{.set}
\alias{.set}
\title{assign key-value pair(s) to a hash }

\description{

 \code{.set} is an internal method for assigning key-value pairs to a 
 \code{\link{hash}}.  Normally, there is no need to use this function.  
 Convenient access is provided by: \code{ \$, [ and [[ } and their 
 corresponding replacement methods.
 
 \code{.set} takes a pair of vectors.  The first is coerced to valid 
 hash keys.  The second servers as values.  

}

\usage{
.set(hash, keys, values )
}

\arguments{
  \item{hash}{ 
    An hash object on which to set the key-value pair(s)
  }
  \item{keys}{ 
    A vector of keys that will be coerced to valid key-value pairs. See
    Details.  
  }
  \item{values}{ A vector of values.  See details. }
}

\details{
    \code{.set} sets zero or more key-value pairs.  If the key(s) already 
    exist, existing values are silently clobbered.  Otherwise, a new 
    value is saved for each key.  Keys and values are determine by the 
    length of these vectors.  If: 


    ... \code{keys} and \code{values} are the same length, key-value pairs are
    added to the hash pairwise.  


    ... \code{keys} is a vector of length 1, then this key is assigned the 
    entire \code{values} vector.


    ... \code{values} is a vector of length 1, each key of \code{keys} is 
    assigned the value given by \code{values} 


    ... \code{keys} and \code{values} are of different lengths, both greater 
    than one, then the assignment is considered ambiguous and an error is
    thrown.

}

\value{
  \code{.set} exists solely for its side-effects.  An invisible NULL is 
  returned.
}

\author{ Christopher Brown }
\note{ 
}
\seealso{ See also \code{\link{hash}}, \code{\link{environment}} }

\keyword{ methods }
\keyword{ data }
\keyword{ manip }

