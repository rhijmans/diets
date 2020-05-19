
setwd("C:/github/diets/scripts")

####
#### Adjusted Food Comp Table
####
source("R/get_FCT.R")
x = getFCT(fbs=FALSE)
y = getFCT(fbs=TRUE)

####
#### Download all FAO FBS, and fix countries, add ISO3 code
####
source("R/get_FBS.R")
get_FBS()

####
#### Population
####
source("R/get_POP.R")
# by year age group
ur <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
get_pop(ur)


####
#### fortification
####

source("R/get_fortification.R")
get_fortif()

####
#### nutrient requirements
####
source("R/get_requirements.R")
r <- getReq(rda=FALSE)
saveRDS(r, "pkg/reqs.rds")
rda <- getReq(rda=TRUE)
saveRDS(rda, "pkg/reqs_rda.rds")
