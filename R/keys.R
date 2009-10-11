# -----------------------------------------------------------------------------
# keys.S
# METHOD: keys
# -----------------------------------------------------------------------------
setGeneric( "keys", function(x) standardGeneric("keys") )
setMethod( "keys" , "hash" ,
	function(x) ls(x@.Data)
)

names.hash <- function(x) keys(x)



