# -----------------------------------------------------------------------------
# METHOD: format 
# -----------------------------------------------------------------------------
setMethod( "format", "hash", 

  function( x, ... ) {

    indent <- list(...)$indent

    if( is.null( indent  )) {
      indent <- "" 
    } else {
      indent <- paste( indent, "    ", sep="" )
    }

    indent2 <- paste( indent, "  ", sep="" )

    ret <- paste( "<hash> containing ", length(x), " key-value pairs.\n", sep="" )


    for ( k in keys(x) ) {
      vals <- paste( format( x[[k]], indent=indent ), collapse = " " ) 
      ret <-  paste( ret, indent2, k, " : ", vals, "\n", sep=""  )    
    }

    ret <- gsub( "\n\n", "\n", ret ) 
    ret

  }
)

                                       


  

