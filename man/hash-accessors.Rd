\name{hash-accessors}

\alias{hash-accessors}
\alias{$,hash-method}
\alias{$<-,hash-method}
\alias{$<-,hash,ANY,ANY-method}
\alias{$<-,hash,ANY,NULL-method}

\alias{[,hash,ANY,missing,missing-method}
\alias{[<-,hash,ANY,missing-method}
\alias{[<-,hash,ANY,missing,ANY-method}
\alias{[<-,hash,ANY,missing,NULL-method}

\alias{[[,hash,ANY,missing-method}
\alias{[[<-,hash,ANY,missing-method}
\alias{[[<-,hash,ANY,missing,ANY-method}
\alias{[[<-,hash,ANY,missing,NULL-method}

\title{Accessor methods for the hash class.}

\description{
  R style accesors for the \code{\link{hash-class}}.
}


\details{
  These are the hash accessor methods. They closely follow an R style.

  \code{$} is a look-up operator for a single key. The key is coerced to valid 
  hash key via \code{\link{make.keys}} returns the corresponding value.  The 
  key name must be literal it is not interpreted.  

  \code{[[} is the look-up, extraction  operator.  It returns the values of 
  a single key. The key names will be interpreted and coerced to a valid hash
  key via \code{\link{make.keys}}.  
  
  \code{[} is a subseting operator.  It returns a (sub) hash with the specified
  keys.  All other keys are removed.  Key names are coerced via \code{\link{make.keys}} 

}

\value{
  
  \$ and [[ return the value for the supplied argument. If a key does not 
  match an existing key, then \code{NULL} is returned with a warning.

  [ returns a hash slice, a sub hash with only the defined keys.

}

\author{ Christopher Brown }

\seealso{ \code{\link{hash}}, \code{\link{.set}}, \code{\link{as.list.hash}} }

\examples{
  h <- hash()
  h <- hash( letters, 1:26 )

  h$a 			
  h$a <- "2"   

  h[[ "a" ]] # c("a","b","c","d") 

  h[ letters[1:4] ]    # hash with a,b,c,d
  h[ letters[1:4] ] <- 4:1

}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
