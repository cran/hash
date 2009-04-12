
# Which is faster setting by mapply or doing a for-loop

size <- 100000
keys <- as.character( sample(1:size) )
values <- as.character( rnorm( size ) )

env.mapply <- new.env( hash = T , parent = emptyenv() )
env.lapply <- new.env( hash = T , parent = emptyenv() )
env.for <- new.env( hash = T , parent = emptyenv() )


system.time(
    mapply( assign, keys, values, MoreArgs = list( envir = env.mapply ) )
)

system.time(
    for( i in 1:length(keys) ) assign( keys[i], values[i], envir = env.for )
)

system.time( 
    lapply( 
      ( 1:length(keys) ) , 
      FUN = function(i) assign( keys[i], values[i], envir = env.lapply )
    )
)


# CONCLUSION:
# Using the for-loop is about 15-20% faster than apply and twice as 
# fast as the mapply


