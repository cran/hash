# -----------------------------------------------------------------------------
# validate-key.R
#
# FUNCTION: validate.key
#
#  Coerces arguments to a valid value that can be be passed to various hash 
#  utilitites.
#  
# -----------------------------------------------------------------------------

validate.key <- function(key) {

	key <- as.character( key )  

	if ( length(key) == 0 ) stop( 
  	    "You must provide at least one key to the hash" 
    )

	if ( any(key=="") ) stop(
	  "\nThe empty character string, '', cannot be used for a key at key(s): ", 
	  paste( which( key == "" ), collapse=", " )
	)	

	return( key )

}


