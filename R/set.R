# ---------------------------------------------------------------------
# METHOD: set.R
#   Sets a key-value pair for the hash object
# 
#  TODO:
#       
# ---------------------------------------------------------------------


# NB, Here we 'assign' the values in a for-loop since this is faster than 
# mapply and apply cnstructions.  See /test/benchmark-set.R for a 
# comparison

.set <-
    function( hash, keys, values ) {
        
        keys <- validate.key(keys)

      # UNEQUAL keys and values both greater than one 	
		if ( 
            length(keys) > 1 & 
            length(values) > 1 & 
            length(keys) != length(values) 
        ) {
			stop( 
				"\nKeys of length ", length( keys ), 
 				" do not match values of length ", length( values )
			)
		}   


        if( length( keys ) == length( values ) ) { 

            for( i in 1:length(keys) )  
                assign( keys[[i]], values[[i]], envir = hash@env )

        } else {

            if( length( keys ) == 1 ) 
                assign( keys, values, envir = hash@env )
            
            if( length( values ) == 1 ) 
                for( i in 1:length(keys) )
                    assign( keys[[i]], values, envir = hash@env )
        }    

        return( invisible(NULL) )

}


