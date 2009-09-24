# ---------------------------------------------------------------------
# values.R
#   values(hash) : returns the values for a hash
# ---------------------------------------------------------------------

setGeneric( "values", function(x, ...) standardGeneric( "values" ) )
setMethod( "values", "hash", 
	function(x, ...) { 
        sapply( keys(x), get, x@env, ... )
	}
) 

