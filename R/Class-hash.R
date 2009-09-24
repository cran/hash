# ---------------------------------------------------------------------
# CLASS: hash
# 
# n.b.: The name of this class from the Open Data R Style Guide which 
# would have the CLASS named 'Hash'.  We use hash since the goal is to 
# emulate a native class that is missing from the R Specification.
# 
# TODO:
#
# CONTAINS: hash(class), hash(constuctor), hash(accessors) $ [ [[
#
# --------------------------------------------------------------------- 
setClass( 
	"hash", 
	representation( env="environment" ) 
)



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

setReplaceMethod(
    "[" ,
    signature(
      x = "hash" ,
      i = "ANY" ,
      j = "missing" ,
      value = "NULL"
    ) ,
    function( x, i, ...,  value ) {
      del( i, x )
      return( x )
    }
)
  


# TEST:
# h[ "foo" ] <- letters # Assigns letters, a vector to "foo"
# h[ letters ] <- 1:26
# h[ keys ] <- value
# 


# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

setMethod( 
  "$" ,
   signature( "hash", "ANY" ) ,
   function( x, name ) {
     get( validate.key( name ), x@env )
   }
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

# CASE: NULL value
setReplaceMethod(
  "$",
  signature=signature(
    x="hash",
    name="ANY",
    value="NULL"
    ),
  function(x, name, value) {
    del(name,x)
    # assign( name, value, envir = x@env )
    x
  }
)


# ---------------------------------------------------------------------
# [[
# ---------------------------------------------------------------------
setMethod( 
  "[[",
  signature( x="hash", i="ANY", j="missing" ) ,
  function(x,i, ...) {
    if( length(i) != 1 ) stop( "Can only one hash value with [[. Several keys were provided" )
    get( validate.key(i), x@env )
  }
)

setReplaceMethod(
  "[[", 
  signature( x="hash", i="ANY", j="missing", value="ANY" ) ,
  function(x,i,value) {
    .set( x, i, value  )
    return( x )
  }
)

# CASE: value=NULL
setReplaceMethod(
  "[[",
  signature( x="hash", i="ANY", j="missing", value="NULL" ) ,
  function(x,i,value) {
    # .set( x, i, value  )
    del( i, x )
    return( x )
  }
)

# ---------------------------------------------------------------------
# MISC. FUNCTIONS
# ---------------------------------------------------------------------

is.hash <- function(x) is( x, "hash" )

as.list.hash <- function(x, all.names=FALSE, ...) as.list( x@env, all.names, ... )




