
source('local-functions.R')


x <- read.csv('soil-parameter-files/soil-parameters.csv')


# load example from defaults
p <- soilParameterFileToList(f = 'soil-parameter-files/defaults/soil_loam.def')

# write to console
writeSoilParameterFile(p)
