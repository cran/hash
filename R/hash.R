# ---------------------------------------------------------------------
# CLASS: hash
# 
# n.b.: The name of this class from the Open Data R Style Guide which 
# would have the CLASS named 'Hash'.  We use hash since the goal is to 
# emulate a native class that is missing from the R Specification.
# 
# TODO:
#   - Add indexing function to emulate an ordered hash.  
#
#   CONTAINS: hash(class), hash(constuctor), hash(accessors) $ [ [[
# --------------------------------------------------------------------- 
setClass( 
	"hash", 
	representation( env="environment" ) 
)


# -----------------------------------------------------------------------------
# CONSTRUCTOR: hash
#   Takes an optional 1 or two parameter
#   DEPENDS on method set
# -----------------------------------------------------------------------------
hash <- function( keys=NULL, values=NULL ) {
	
  # INITIALIZE A NEW HASH   
	h <- new( 
        "hash", 
        env = new.env( hash = TRUE , parent=emptyenv() )  
    )

    if ( ! is.null(keys) && ! is.null(values) ) 
        .set( h, keys, values )

	return(h)
}
	


# ----------------------- ACCESSOR METHODS ------------------------------

  # --------------------------------------------------------------------- 
  # METHOD: [ (hash slice)
  #   The [ method provides for the subseting of the object and 
  #   extracting a copy of the slice.
  #
  #   Provides access to the hash.  
  #   How to handle missing keys, error, NA
  # --------------------------------------------------------------------- 
  setMethod(
	  "[" ,
		signature( 
			x="hash" ,
			i="ANY"  ,
			j="missing" ,
			drop = "missing"
		) ,
		function(x,i,j, ..., drop) {
			return( hash( i, .get( x, i, ... ) ) )
  		}
	)	

# --------------------------------------------------------------------- 
# METHOD: [<-, Hash Slice Replacement Method
# WHAT DO WE DO IF WE HAVE A DIFFERENT NUMBER OF KEYS AND VALUES?
#   This should implement a hash slice.
# --------------------------------------------------------------------- 
# getGeneric("[<-")
setReplaceMethod(
	"[" ,
	signature(
		x = "hash" ,
		i = "ANY" ,
		j = "missing" ,
    value = "ANY" 
	) ,
	function( x, i, ...,  value ) {
	  .set( x, i, value, ...  )
	  return( x )
    }
)
  


# TEST:
# h[ "foo" ] <- letters # Assigns letters, a vector to "foo"
# h[ letters ] <- 1:26
# h[ keys ] <- value


# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

setMethod( 
  "$" ,
   signature( "hash", "ANY" ) ,
   function( x, name ) .get( x, validate.key(name) )
)
  

setReplaceMethod(
	"$",
  signature=signature(
  	x="hash",
  	name="ANY",
  	value="ANY"
	),
  function(x, name, value) {
    assign( name, value, envir = x@env )
    x
  }
) 

# j <- new( "hash" )

# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
setMethod( 
  "[[",
  signature( x="hash", i="ANY", j="missing" ) ,
  function(x,i, ...) 
    return( .get( x, i, ...) ) 
)

setReplaceMethod(
  "[[", 
  signature( x="hash", i="ANY", j="missing", value="ANY" ) ,
  function(x,i,value) {
    .set( x, i, value  )
    return( x )
  }
)

