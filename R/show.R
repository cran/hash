# -----------------------------------------------------------------------------
# show.r
# 
# METHODS: show 
#  The default method on the class.  Perhaps this should return the 
#  length.
#
# -----------------------------------------------------------------------------
setMethod( "show" , "hash" ,
	function(object) {
		cat( 
			paste( 
				"An object of type 'hash' containing",
				length( object ) ,
				"key-value pairs.\n"
			)
		)
	}
) 
