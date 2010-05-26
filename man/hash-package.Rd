\name{hash-package}
\alias{hash-package}
\docType{package}

\title{ Hash/associative array/dictionary data structure for the R language. }

\description{

    This S4 class is designed to provide a hash-like data structure in a  
    native R style and provides the necessary methods for all general 
    methods for hash operations.

}

\details{
  \tabular{ll}{
    Package: \tab hash\cr
    Type: \tab Package\cr
    Version: \tab 2.0.1\cr
    Date: \tab 2010-05-26\cr
    License: \tab LGPL 2+\cr
    LazyLoad: \tab yes\cr
    Depends: \tab R (>= 2.10.0), utils, methods\cr
  }
}

\author{
Christopher Brown

Maintainer: Christopher Brown <cbrown -at- opendatagroup -dot- com>
}

\references{

    http://opendatagroup.com/2010/04/26/hash-2-0-0/

    http://opendatagroup.com/2009/07/26/hash-package-for-r/

    http://opendatagroup.com/2010/02/17/hash-1-99-x/
 
    http://opendatagroup.com/2009/10/21/r-accessors-explained/

    http://www.mail-archive.com/r-help@r-project.org/msg37637.html

    http://www.mail-archive.com/r-help@r-project.org/msg37650.html

    http://tolstoy.newcastle.edu.au/R/help/05/12/index.html\#18192

}


\note{ 
  
  R is slowly moving toward a native implementation of hashes 
  using enviroments, (cf. \code{\link{Extract}}. Access to 
  environments using $ and [[ has been available for some time and
  recently objects can inherit from environments, etc. But many features
  that make hashes/dictionaries great are still lacking, such
  as the slice operation, [.

  The hash package is the only full featured hash 
  implementation for the R language. It provides more features and 
  finer control of the hash behavior than the native feature set and 
  has similar and sometimes better performance.

  PERFORMANCE.  Hashes are based on R's native environments and are 
  designed to be exceedingly fast using the environments internal 
  hash table.  For small, 
}


\seealso{
  See also \code{\link{hash} }, \code{\link{hash-accessors}} and 
  \code{\link{environment} }
}

\examples{

  h <- hash( keys=letters, values=1:26 )
  h <- hash( letters, 1:26 )

  h$a # 1

  h$foo <- "bar"
  h[ "foo" ] 
  h[[ "foo" ]]

  clear(h)
  rm(h)

}

\keyword{ package }

