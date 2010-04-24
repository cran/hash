\name{hash-class}
\docType{class}
\alias{hash-class}

\title{Class "hash" }
\description{ 

  Implements a S4 hash class in R similar to hashes / associatesd arrays / 
  dictionaries in other programming languages.  Where possible, the hash
  class uses the standard R accessors:  \code{\$}, \code{[} and \code{[[}. 
  Hash construction is flexible and takes several syntaxes and all hash 
  operations are supported.

  For shorter key-value pairs, lists might yield higher performance, but for
  lists of appreciable length hash objects handly outperform native lists.
    
}

\section{Slots}{
  \describe{ 
     \item{\code{.xData}:}{ 
       Object of class \code{"environment"}.  This is the hashed environment 
       used for key-value storage. }
  }
}

\section{Extends}{
  environment
}


\section{Methods}{
  \describe{
    HASH ACCESSORS:
    \item{[<-}{\code{signature(x = "hash", i = "ANY", j = "missing")}: Slice Replacement }
    \item{[}{\code{signature(x = "hash", i = "ANY", j = "missing", drop = "missing")}
: Slice }

    \item{[[<-}{\code{signature(x = "hash", i = "ANY", j = "missing")}: Single key replacement with interpolation.  }
    \item{[[}{\code{signature(x = "hash", i = "ANY", j = "missing")}: Single key look-up with interpolation  }

    \item{\$<-}{\code{signature(x = "hash")}: Single key replacement no interpolation }
    \item{\$}{\code{signature(x = "hash")}: Single key lookup no interpolation }

    Manipulation:
    \item{clear}{\code{signature(x = "hash")}: Remove all key-value pairs from hash }
    \item{del}{\code{signature(x = "ANY", hash = "hash")}: Remove specified key-value pairs from hash }
    \item{has.key}{\code{signature(key = "ANY", hash = "hash")}: Test for existence of key }
    \item{is.empty}{\code{signature(x = "hash")}: Test if no key-values are assigned }  

    \item{length}{\code{signature(x = "hash")}: Return number of key-value pairs from the hash }
    \item{keys}{\code{signature(hash = "hash")}: Retrieve keys from hash }
    \item{values}{\code{signature(x = "hash")}: Retrieve values from hash }

    \item{copy}{\code{signature(x = "hash")}: Make a copy of a hash using a new environment. }

    \item{format}{\code{signature(x = "hash")}: Internal function for displaying hash }

  }
}

\references{ 

	http://en.wikipedia.org/wiki/Hash_table

	http://en.wikipedia.org/wiki/Associative_array

}
\author{ Christopher Brown }

\note{ 

  Hash keys must be a valid character value and may not be the empty 
  string \code{""}. 

  Hash values can be any R value, vector or object. 

  Because hash values are reference objects in R, all copies will 
  refer to the original. Changes on the copies will be reflected in the
  original.

  For named lists and vectors of appreciable length hashes provide much quicker 
  element access. Since hashes are based on environments, environment performance
  will always exceed that of a hash object, but will lack all the requisite 
  functions. 

  Objects of class \code{hash} do not release memory with a call to 
  \code{rm}.  \code{clear} must be called before \code{rm} to properly
  release the memory.

}

\seealso{
	\code{\link{hash-accessors}}, \code{\link{environment}}. 
}

\examples{
  showClass("hash")
}

\keyword{classes}
