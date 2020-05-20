
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
get_pop()

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
rda <- getRe
q(rda=TRUE)
saveRDS(rda, "pkg/reqs_rda.rds")

