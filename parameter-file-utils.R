library(aqp)
library(soilDB)
library(reshape2)

source('local-functions.R')


x <- read.csv('soil-parameter-files/soil-parameters.csv')


# example component
s <- fetchSDA(WHERE = "cokey = '22185922'", duplicates = TRUE, childs = FALSE)



# load example from defaults
p <- soilParameterFileToList(f = 'soil-parameter-files/defaults/soil_loam.def')

# write to console
writeSoilParameterFile(p)

# modify using SSURGO component
buildParameterList(s, template = p)

