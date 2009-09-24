#  ---------------------------------------------------------------------
# length.R
#   return the number of keys in a hash
#  ---------------------------------------------------------------------

setMethod( "length" , "hash" ,
    function(x) length( x@env )  
)


