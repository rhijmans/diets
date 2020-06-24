
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("C:/github/diets/scripts")
} else {
	setwd("C:/Users/jccaro/diets/")
}


dir.create("pkg", FALSE)
dir.create("process", FALSE)
####
#### Adjusted Food Comp Table
####
source("R/get_FCT.R")
getFCT()

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
getReq()

