# -----------------------------------------------------------------------------
# keys.S
# METHOD: keys
# -----------------------------------------------------------------------------
setGeneric( "keys", function(hash) standardGeneric("keys") )
setMethod( "keys" , "hash" ,
	function(hash) ls(hash@env)
)

