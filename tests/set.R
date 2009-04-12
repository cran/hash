
# TEST FILE FOR hash METHODS set

library(hash)
h <- hash()

# SET: key-value pairs
.set( h, "a", 1:2 )
.set( h, letters, 1:26  )
.set( h, 1:5, 1:5 ) 
.set( h, letters, 12  )

        
# SET: data.frame

# SET: list

# SET: environment
