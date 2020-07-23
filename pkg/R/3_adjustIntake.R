


add_Ca <- function(intake, consumption=1.7, concentration=0.042) {
	## Add Calcium in Water
	d <- intake[1,]
	d[1,] <- NA
	d$group = "water"
	d$tag = "CA"
	#d$desc = "Calcium, Ca"
	d$value = concentration
	d$mass = consumption
	#d$unit = "permille"
	d$intake = d$value * d$mass
	#d$code = -1
	intake <- rbind(intake,d)
}


adjust_Ca <- function(intake) {
	## Calculation of the animal protein
	prot  <- intake[intake$tag == "PROCNT",]
	Aprot <- prot[prot$code >= 2700 & prot$code <= 2800,]
	## RH should we exclude honey? 
	## Aprot <- Aprot[prot$group  != "honey", ]
	AP  <- sum(Aprot$intake, na.rm = TRUE)
	## Choose the right requirement depending on the animal protein
	i  <- intake$tag == "CA"
	if (AP < 40) {
		intake$intake[i] <- intake$intake[i] * 1.263
	} else if (AP <= 60) {
		intake$intake[i] <- intake$intake[i] * 1.108
	}
	# the above could easily be made continuous with approx
	
	intake
}


# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1995555/
# note that the units for TDZ and TDP is mmol/day
# phytate = 660.04 g/mol
# Zn = 65.38 g/mol
# What is the unit of phytate?

.miller <- function(TDZ, TDP, Amax=0.13, Kr=0.1, Kp=1.2) {
	TDZ <- 1000 * TDZ / 65.38
	TDP <- TDP / 660.04
	(0.5 / TDZ) * ((Amax + TDZ + Kr *(1 + TDP / Kp)) - 
		sqrt((Amax + TDZ + Kr * (1 + TDP / Kp))^2 - 4 * Amax * TDZ))
}


adjust_Zn <- function(intake) {
	## Create variables with Phytate and Zinc values.
	i  <- intake$tag == "PHYTAC"
	phytate  <-  sum(intake$intake[i], na.rm = TRUE)
	i  <- intake$tag == "ZN"
	zinc  <- sum(intake$intake[i])
	FAZ <- .miller(zinc, phytate)

	## Multiply fractional absorption for zinc with the zinc value
	intake$intake[i]  <- intake$intake[i] * FAZ
	intake
}



adjust_Fe <- function(intake, heme) {

	## Calculation of the iron bio-availability factor
	prot  <- intake[intake$tag == "PROCNT",]
	
	#i <- prot$code %in% c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2765", "2766", "2767", "2768", "2769")
	i <- prot$group %in% c('Bovine Meat', 'Freshwater Fish', 'Marine Fish, Other', 'Meat, Other', 'Mutton & Goat Meat', 'Offals, Edible', 'Pigmeat', 'Poultry Meat')
	prot  <- prot[i,]
	
	MeatP  <- sum(prot$mass, na.rm = TRUE)
	i  <- intake$tag == "PHYTAC"
	Phytate  <- sum(intake$intake[i], na.rm = TRUE) * 1000
	i <- intake$tag == "VITC"
	VitC <- sum(intake$intake[i], na.rm = TRUE) * 1000

	### Calculation of the availibility of non Heme Iron
	## Import MeatP, vitC and Phytate groups.
	#NH  <- readRDS(system.file("ex", "NHemeIron.rds", package="diets"))
	NH <- heme
	i <- NH$MMeat > MeatP & NH$LMeat < MeatP
	x <- ifelse(any(i), NH$Index[i], 1)
	j <- Phytate < NH$MPhytate & Phytate > NH$LPhytate
	y <- ifelse(any(j), NH$Index[j], 1)
	k <- VitC < NH$MVitaminC & VitC > NH$LVitaminC
	z <- ifelse(any(k), NH$Index[k], 1)
	### BioNoHeme
	## Using group to create an index (between 0 and 1) for nonHeme Iron
	Ind  <-  (x * 40 + (1 - y) * 40 + z * 20) / 100
	## Assuming that max bioavaliability is 18 then 18 * (0-1) to have a normalized scale.
	BioNoheme  <- Ind * 18

	## Calculation of the part of Heme and non Heme Iron
	Iron  <- intake[intake$tag == "FE",]
	## Parts of heme iron in differents types of meats
	Heme  <- data.frame(first = c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2766", "2767", "2768", "2769"), second = c(65, 72, 39, 26, 40, 40, 26, 26, 26, 26, 40, 40, 40, 40), stringsAsFactors = FALSE)
	Iron  <- merge(Iron, Heme, by.x = "code", by.y= "first" , all.x = TRUE)
	i <- is.na(Iron$tag)
	Iron[i,"tag"]  <- 0
	Iron$Heme  <- Iron[,"intake"] * Iron[,"second"] / 100
	Heme  <- sum(Iron$Heme, na.rm = TRUE)
	NonHeme  <- sum(Iron[,"intake"], na.rm = TRUE) - Heme

	### Calculation of the total bioavailiabity of Iron
	Bioavail  <- (Heme * 25 + NonHeme *  BioNoheme) / sum(Iron$intake, na.rm = TRUE)

	i <- intake$tag == "FE"
	## Making 5 group of bioavaliability
	if (Bioavail <= 6.25) {
		intake$intake[i] <- intake$intake[i] / 3.055
	} else if (Bioavail < 8.75) {
		intake$intake[i] <- intake$intake[i] / 2.249
	} else if (Bioavail < 11.25) {
		intake$intake[i] <- intake$intake[i] / 1.506
	} else if (Bioavail < 13.75) {
		intake$intake[i] <- intake$intake[i] / 1.248
	} #else {
		#intake$intake[i]  <- intake$intake[i]
	#}	
	
	# this last part would be more elegant if we did something like 
	#x <- c(0, 6.25, 8.75, 11.25, 13.75)
	#y <- c(3.055, 2.249, 1.506, 1.248, 1) 
	# adj <- approx(x, y, xout=Bioavail, rule=2)$y
    # intake$intake[i] <- intake$intake[i] / adj
    # but I do not know where to put the x values (in the middle?)
    # need to see what this was derived from	
	
	intake
}




