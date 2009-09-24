
.onLoad <- function( libname, pkgname ) {

  cat( "\nThe ", pkgname, " package provided by:\n" ) 
  width <- getOption("width")
  options( width=80 )

  cat(
  "                            _       _        ",
  "  ___  _ __   ___ _ __   __| | __ _| |_ __ _ ",
  ' / _ \\| "_ \\ / _ \\ "_ \\ / _` |/ _" | __/ _" |',
  "| (_) | |_) |  __/ | | | (_| | (_| | || (_| |",
  ' \\___/| .__/ \\___|_| |_|\\__,_|\\__,_|\\__\\__,_|',
  "      |_|   http://www.opendatagroup.com      \n",
  sep="\n"
  )
  
  options( width=width )

}


