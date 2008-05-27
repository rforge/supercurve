library(SuperCurve)

path <- system.file("rppaCellData", package="SuperCurve")

try( RPPA("cellLineInfo.tsv") )
try( RPPA("cellLineInfo.tsv", path=path) )

try( RPPA("Akt.txt", path=path, blanks='blank') )
try( RPPA("Akt.txt", path=path, blanks=700:710) )

akt <- RPPA("Akt.txt", path=path)
image(akt, colorbar=TRUE)

try( image(akt, colorbar=1) ) # the error checking is wrong here
try( image(akt, colorbar='red') ) # okay here?

try( image(akt, measure="bogus") )

windows(width=1.2, height=6)
try( image(akt) )
try( image(akt, colorbar=TRUE) )

