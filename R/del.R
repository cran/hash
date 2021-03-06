#' del, delete - remove key-value pair(s) from a hash
#' 
#' Removes key-value pair(s) from a hash by name of the object. There are also 
#' R-like methods described in \code{link{Extract}}. To delete all 
#' keys, use the \code{\link{clear}} method.
#' 
#' @param x An object that will be coerced to valid key(s) to be removed from
#' the hash.  \code{x} will be coerced to a valid hash keys using
#' \code{\link{make.keys}}
#' @param hash A \code{\link{hash}} object
#' @return None. This method exists solely for the side-effects of removing
#' items from the hash.
#' 
#' @author Christopher Brown
#' 
#' @seealso
#'    \code{\link[base]{rm}} base function used by \code{del}
#'    \code{\link{Extract}} for R-like accessor
#'    \code{\link{clear}} to remove all key-values and return an empty hash
#'    \code{\link{hash}}
#'  
#' @keywords methods data manip
#' @examples
#' 
#'   h <- hash( letters, 1:26 )
#'   
#'   # USING del 
#'   del( "a", h )             # delete key  a
#'   del( c("b","c"), h )      # delete keys b, c
#'   
#'   # USING rm  
#'   rm( "d", envir=h )              # delete key  d
#'   rm( list= c("e","f"), envir=h ) # delete keys e,f
#'   
#'   # USING R syntsx
#'   h$g <- NULL               # delete key  g
#'   h[['h']] <- NULL          # delete key  h
#'   h['i'] <- NULL            # delete key  i
#'   h[ c('j','k')] <- NULL    # delete keys e,f
#'    
#'   keys(h)
#'   D
#' @name del
#' @aliases delete del-methods delete-methods
#' @rdname del
#' @docType methods
#' @export

setGeneric( "del", function( x, hash ) { standardGeneric("del") } )


#' @name del,ANY,hash-method
#' @rdname del

setMethod( 
	"del" , c( "ANY", "hash" ) ,
	function ( x, hash ) 
	  rm( list=make.keys(x), envir=hash@.Data )
)


#' @name del,character,hash-method
#' @rdname del

setMethod( "del", c( 'character', 'hash' ),
  function( x, hash ) 
    rm( list=x, envir=hash@.Data)
)


#' @name delete
#' @aliases delete
#' @rdname del
#' @export

setGeneric( "delete", function( x, hash ) { standardGeneric("delete") } )


#' @rdname del
#' @aliases delete,ANY,hash-method

setMethod(
  "delete",
  signature( "ANY", "hash" ) ,
    function(x,hash) { del(x,hash) }
)


