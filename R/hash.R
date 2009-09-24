# -----------------------------------------------------------------------------
# CONSTRUCTOR: hash
#   Takes an optional 1 or two parameter
#   DEPENDS on method set
# -----------------------------------------------------------------------------
hash <- function( ... ) {

  li <- list(...)  

  # INITIALIZE A NEW HASH   
  h <- new( 
    "hash", 
     env = new.env( hash = TRUE , parent=emptyenv() )  
  )

  if ( length(li) >  0  ) { 
  
    # TODO: construct from hash and environment objects.
    # if ( class(li[[1]]) == 'hash' ) {
    #  h <- li[[1]]
    #  li <- li[-1]
    # } else 
    # 
    #if ( class(li[[1]]) == 'environment' ) {
    #  h <- new( "hash", env=li[[1]] )
    #  li <- li[-1]
    #}

    if( length(li) > 0 ) .set( h, ... )

  }

  return(h)

}
	


