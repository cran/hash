# ---------------------------------------------------------------------
# del.R
#
# METHOD: del
#   Remove a list of keys.
# ---------------------------------------------------------------------
setGeneric( "del", function( x, hash ) { standardGeneric("del") } )

setMethod( 
	"del" ,
	signature( "ANY", "hash" ) ,
	function ( x, hash ) {
		rm( list=validate.key(x), envir=hash@env )
	}
)

#  ALIAS delete
setGeneric( "delete", function( x, hash ) { standardGeneric("delete") } )
setMethod(
  "delete",
  signature( "ANY", "hash" ) ,
    function(x,hash) { del(x,hash) }
)
