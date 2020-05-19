
adjust_intake_ca  <-  function(intake) {
	## Add Calcium in Water
	d  <- data.frame("CA", 0.0714, 99999, "Water", 0.042, 1.7)
	colnames(d) <- colnames(intake)
	intake  <- rbind(intake,d)
		## Calculation of the animal protein
	Prot  <- intake[intake[, "Tagname"] == "PROCNT",]
	AP  <- Prot[Prot[,"Code_FdGp1"] >= 2700 & Prot[,"Code_FdGp1"] <= 2800,]
	AP  <- sum(AP[,"MNutr_PersonDay"], na.rm = TRUE)

	## Choose the right requirement depending on the animal protein
	if(AP < 40) {
		i  <- intake[,"Tagname"] == "CA"
		intake[i, "MNutr_PersonDay"] <- intake[i, "MNutr_PersonDay"] * 1.263
	} else if(AP >= 40 & AP <= 60) {
		i  <- intake[,"Tagname"] == "CA"
		intake[i, "MNutr_PersonDay"] <- intake[i, "MNutr_PersonDay"] * 1.108
	}
	intake
}


adjust_intake_fe  <-  function(intake) {
	## Calculation of the iron bioavaliability factor
	Prot  <- intake[intake[,"Tagname"] == "PROCNT",]
	Prot  <- Prot[Prot[,"Code_FdGp1"] %in% c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2765", "2766", "2767", "2768", "2769"),]
	MeatP  <- sum(Prot[,"Mass"], na.rm = TRUE)
	i  <- intake[,"Tagname"] == "PHYTAC"
	Phytate  <- sum(intake[i, "MNutr_PersonDay"], na.rm = TRUE) * 1000
	i  <- intake[,"Tagname"] == "VITC"
	VitC <- sum(intake[i,"MNutr_PersonDay"], na.rm = TRUE) * 1000

	### Calculation of the availibility of non Heme Iron
	## Import MeatP, vitC and Phytate groups.
	NH  <- readRDS(system.file("external", "NHemeIron.csv", package="diets"))
	
	NH$MMeat[1] <- 5000 #RH for crazy cases
	i  <- NH$MMeat > MeatP & NH$LMeat < MeatP
	x <- NH$Index[i]
	j  <- Phytate < NH$MPhytate & Phytate > NH$LPhytate
	y <- NH$Index[j]
	k  <- VitC < NH$MVitaminC & VitC > NH$LVitaminC
	z <- NH$Index[k]

	### BioNoHeme
	## Using group to create an index (between 0 and 1) for nonHeme Iron
	Ind  <-  (x * 40 + (1 - y) * 40 + z * 20) / 100
	## Assuming that max bioavaliability is 18 then 18 * (0-1) to have a normalized scale.
	BioNoheme  <- Ind * 18

	## Calculation of the part of Heme and non Heme Iron
	Iron  <- intake[intake[,"Tagname"] == "FE",]
	## Parts of heme iron in differents types of meats
	Heme  <- data.frame(first = c("2731", "2732", "2733", "2734", "2735", "2736", "2761", "2762", "2763", "2764", "2766", "2767", "2768", "2769"), second = c(65, 72, 39, 26, 40, 40, 26, 26, 26, 26, 40, 40, 40, 40), stringsAsFactors = FALSE)
	Iron  <- merge(Iron, Heme, by.x = "Code_FdGp1", by.y= "first" , all.x = TRUE)
	i <- is.na(Iron[,"Tagname"])
	Iron[i,"Tagname"]  <- 0
	Iron$Heme  <- Iron[,"MNutr_PersonDay"] * Iron[,"second"] / 100
	Heme  <- sum(Iron$Heme, na.rm = TRUE)
	NonHeme  <- sum(Iron[,"MNutr_PersonDay"], na.rm = TRUE) - Heme

	### Calculation of the total bioavailiabity of Iron
	Bioavail  <- (Heme * 25 + NonHeme *  BioNoheme) / sum(Iron[,"MNutr_PersonDay"], na.rm = TRUE)

	## Making 5 group of bioavaliability
	if (Bioavail < 13.75 && Bioavail >= 11.25) {
		i  <- intake[, "Tagname"] == "FE"
		intake[i, "MNutr_PersonDay"] <- intake[i, "MNutr_PersonDay"] / 1.248
	} else if (Bioavail < 11.25 && Bioavail >= 8.75) {
		  i  <- intake[, "Tagname"] == "FE"
		  intake[i, "MNutr_PersonDay"]  <- intake[i, "MNutr_PersonDay"] / 1.506
	} else if (Bioavail < 8.75 && Bioavail >= 6.25) {
		  i  <- intake[, "Tagname"] == "FE"
		  intake[i, "MNutr_PersonDay"]  <- intake[i, "MNutr_PersonDay"] / 2.249
	} else if (Bioavail <= 6.25) {
		  i  <- intake[, "Tagname"] == "FE"
		  intake[i, "MNutr_PersonDay"]  <- intake[i, "MNutr_PersonDay"] / 3.055
	} else {
		  i  <- intake[, "Tagname"] == "FE"
		  intake[i, "MNutr_PersonDay"]  <- intake[i, "MNutr_PersonDay"]
	}
	
	intake
}





adjust_intake_fortification  <-  function(intake, fortif){

    ## Open table with fortification by Country
 #   frtf  <- .fortif()
 #   frtf <- frtf[frtf$ISO3 == ISO3,]

    ## Calculate the mass of cooking Oil (all the oils)
    intakeCA <- intake[intake[,"Tagname"] == "CA",c("Code_FdGp1","Mass")]
    CkOil <- intakeCA[intakeCA[,"Code_FdGp1"] %in%  c(2571, 2572, 2573, 2574, 2575, 2576, 2577, 2578, 2579, 2580, 2581, 2582, 2586),]
    CkOil <- sum(CkOil[,"Mass"], na.rm = TRUE)

    ## Merge with intake to add the mass and calculating micronutrient mass for this country/Year
    frtf <- merge(frtf, intakeCA, by.x = c("Code_FdGp"), by.y = "Code_FdGp1", all.x = TRUE)
    i <- frtf$Crop == "Cooking Oil"
    frtf$Mass[i] <- CkOil
    frtf$SumVal <- (frtf$Values * frtf$Mass) / 1000

    ## bind Fortication with intake
    frtf <- frtf[,c("Tagname", "SumVal", "Code_FdGp", "Crop", "Values", "Mass")]
    colnames(frtf) <- colnames(intake)
    intake <- rbind(intake,frtf)
    intake <- aggregate(intake[,c("MNutr_PersonDay", "MNutr_Val", "Mass"), drop = FALSE], intake[,c("Tagname", "Code_FdGp1")], sum)
  
  return(intake)
}

