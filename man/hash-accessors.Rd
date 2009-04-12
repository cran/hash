\name{hash-accessors}

\alias{hash-accessors}
\alias{$,hash-method}
\alias{$<-,hash-method}
\alias{[,hash,ANY,missing,missing-method}
\alias{[<-,hash,ANY,missing-method}
\alias{[[,hash,ANY,missing-method}
\alias{[[<-,hash,ANY,missing-method}

\title{Accessor methods for the hash class.}

\description{

  R style accesors for the \code{\link{hash-class}}.
  
}


\details{

  These are the hash accessor methods. They closely follow an R style.

}

\value{
  
  \$ returns the hash entry for the supplied key.

  [ returns a hash slice, a sub hash with only the defined keys.

  [[ returns a list of values.
  
}

\author{ Christopher Brown }
\seealso{ \code{\link{hash}}, \code{\link{.set}} }

\examples{
    h <- hash()
    h <- hash( letters, 1:26 )

	h$a 			# 1
	h$a <- "2"   

	h[ letters[1:4] ]    # hash with a,b,c,d
	h[ letters[1:4] ] <- 4:1
	
	h[[ letters[1:4] ]] # c("a","b","c","d") 

}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
