# ---------------------------------------------------------------------
# get.R 
#
#  Supplied the hidden function '.get' for retrieving the values 
#  associated with the supplied keys,
# 
#  Experimental behavior:
#  - If no supplied keys are given then returns all the values?
#    This will affect hash slices.
#  ? If ask to retrieve the keys of that don't exist ?
#    - Error.
#    - Fail Silently.
#    - Return NULL or NA for those keys
#
#  See Also: values and various accessor functions
#   
# 
#   TODO: 
#    - Deprecate in favor of $ [ [[ accessors
#    - Decide on behavior when one key is not available ... rm?
# ---------------------------------------------------------------------

.get <- 
  function( hash, keys, drop=TRUE,... ) {
    g <- sapply( validate.key(keys), get, env=hash@env, ... )
    if( drop ) g <- drop(g)
    return(g)
  } 



