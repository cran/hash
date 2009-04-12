# -----------------------------------------------------------------------------
# items.R
# 
# METHOD: items
# 
# Coerces a hash into a list.  Retained for similarity to Python dictionaries
#
# -----------------------------------------------------------------------------

setGeneric( "items", function(x) standardGeneric("items"), useAsDefault=F )

setMethod(
  "items" ,
  signature(x = "hash" ),
  function(x) as.list(h@env)
)
