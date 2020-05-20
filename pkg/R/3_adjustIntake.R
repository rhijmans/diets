
# Miller
adjust_zinc <- function(intake) {
	## Create variables with Phytate and Zinc values.
	i  <- intake$tag == "PHYTAC"
	phytate  <-  sum(intake$intake, na.rm = TRUE)
	i  <- intake$tag == "ZN"
	zinc  <- sum(intake$intake[i])

	## Calculate mineral fractional absorption for zinc (Miller)
	FAZ  <- (0.5/ zinc) * ((0.13 + zinc + 0.1 *(1 + phytate / 1.2)) -  sqrt((0.13 + zinc + 0.1 * (1 + phytate / 1.2))^2 - 4 *(0.13 * zinc)))
	## Multiply fractional absorption for zinc with the zinc value
	i  <- intake$tag == "ZN"
	intake$intake[i]  <- intake$intake[i] * FAZ
	intake
}



adjust_iron <- function(intake, heme) {
	## Calculation of the iron bioavailability factor
	prot  <- content[intake$tag == "PROCNT",]
	
	#i <- prot$code %in% c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2765", "2766", "2767", "2768", "2769")
	i <- prot$group %in% c("Bovine Meat', 'Freshwater Fish', 'Marine Fish, Other', 'Meat, Other', 'Mutton & Goat Meat', 'Offals, Edible', 'Pigmeat', 'Poultry Meat")
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
	NH$MMeat[1] <- 5000 #RH for crazy cases
	i <- NH$MMeat > MeatP & NH$LMeat < MeatP
	x <- NH$Index[i]
	j <- Phytate < NH$MPhytate & Phytate > NH$LPhytate
	y <- NH$Index[j]
	k <- VitC < NH$MVitaminC & VitC > NH$LVitaminC
	z <- NH$Index[k]

	### BioNoHeme
	## Using group to create an index (between 0 and 1) for nonHeme Iron
	Ind  <-  (x * 40 + (1 - y) * 40 + z * 20) / 100
	## Assuming that max bioavaliability is 18 then 18 * (0-1) to have a normalized scale.
	BioNoheme  <- Ind * 18

	## Calculation of the part of Heme and non Heme Iron
	Iron  <- content[content[,"tag"] == "FE",]
	## Parts of heme iron in differents types of meats
	Heme  <- data.frame(first = c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2766", "2767", "2768", "2769"), second = c(65, 72, 39, 26, 40, 40, 26, 26, 26, 26, 40, 40, 40, 40), stringsAsFactors = FALSE)
	Iron  <- merge(Iron, Heme, by.x = "Code_FdGp1", by.y= "first" , all.x = TRUE)
	i <- is.na(Iron[,"tag"])
	Iron[i,"tag"]  <- 0
	Iron$Heme  <- Iron[,"intake"] * Iron[,"second"] / 100
	Heme  <- sum(Iron$Heme, na.rm = TRUE)
	NonHeme  <- sum(Iron[,"intake"], na.rm = TRUE) - Heme

	### Calculation of the total bioavailiabity of Iron
	Bioavail  <- (Heme * 25 + NonHeme *  BioNoheme) / sum(Iron[,"intake"], na.rm = TRUE)

	i <- content$tag == "FE"
	## Making 5 group of bioavaliability
	if (Bioavail <= 6.25) {
		content$intake[i] <- content$intake[i] / 3.055
	} else if (Bioavail < 8.75) {
		content$intake[i] <- content$intake[i] / 2.249
	} else if (Bioavail < 11.25) {
		content$intake[i] <- content$intake[i] / 1.506
	} else if (Bioavail < 13.75) {
		content$intake[i] <- content$intake[i] / 1.248
	} else {
		#content$intake[i]  <- content$intake[i]
	}	
	content
}



adjust_ca <- function(intake) {
	## Add Calcium in Water
	d  <- data.frame("CA", 0.0714, 99999, "Water", 0.042, 1.7)
	colnames(d) <- colnames(intake)
	intake <- rbind(intake,d)
	
	## Calculation of the animal protein
	Prot <- intake[intake[, "tag"] == "PROCNT",]
	AP   <- Prot[Prot[,"group"] >= 2700 & Prot[,"group"] <= 2800,]
	AP   <- sum(AP[,"intake"], na.rm = TRUE)

	## Choose the right requirement depending on the animal protein
	if (AP < 40) {
		i  <- intake[,"tag"] == "CA"
		intake[i, "intake"] <- intake[i, "intake"] * 1.263
	} else if (AP >= 40 & AP <= 60) {
		i  <- intake[,"tag"] == "CA"
		intake[i, "intake"] <- intake[i, "intake"] * 1.108
	}
	intake
}



adjust_fortify <-  function(intake, fortif) {

    ## Open table with fortification by Country
 #   frtf  <- .fortif()
 #   frtf <- frtf[frtf$ISO3 == ISO3,]

    ## Calculate the mass of cooking Oil (all the oils)
    intakeCA <- intake[intake[,"tag"] == "CA",c("group", "mass")]

    #CkOil <- intakeCA[intakeCA[,"group"] %in%  c(2571, 2572, 2573, 2574, 2575, 2576, 2577, 2578, 2579, 2580, 2581, 2582, 2586),]

    CkOil <- intakeCA[intakeCA$group %in% c("Coconut Oil', 'Cottonseed Oil', 'Groundnut Oil', 'Maize Germ Oil', 'Oilcrops Oil, Other', 'Olive Oil', 'Palm Oil', 'Palmkernel Oil', 'Rape and Mustard Oil', 'Ricebran Oil', 'Sesameseed Oil', 'Soyabean Oil', 'Sunflowerseed Oil"), ]

	CkOil <- sum(CkOil[,"mass"], na.rm = TRUE)

    ## Merge with intake to add the mass and calculating micronutrient mass for this country/Year
    frtf <- merge(frtf, intakeCA, by.x = c("Code_FdGp"), by.y = "group", all.x = TRUE)
    i <- frtf$Crop == "Cooking Oil"
    frtf$Mass[i] <- CkOil
    frtf$SumVal <- (frtf$Values * frtf$Mass) / 1000

    ## bind fortification with intake
    frtf <- frtf[,c("tag", "SumVal", "Code_FdGp", "Crop", "Values", "mass")]
    colnames(frtf) <- colnames(intake)
    intake <- rbind(intake,frtf)
    intake <- aggregate(intake[,c("intake", "MNutr_Val", "mass"), drop = FALSE], intake[,c("tag", "group")], sum)
  
	return(intake)
}


