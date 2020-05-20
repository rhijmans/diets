
## Calculation of the average micronutrient intake per person for a country

nutrientIntake  <-  function(consumption, content, byEnergy=TRUE, Miller=TRUE, Wg){
	
	#stopifnot(c("code_FdGp1", "Item_FdGp1", "Tagname", "MNutr_Val") %in% colnames(content))
	stopifnot(c("code", "tag", "value") %in% colnames(content))
	#stopifnot(c("Code_FdGp1") %in% colnames(consumption))


  ### PART 1 : Compute mass and micronutrient by person per day from content and consumption according to the value of use.
	if (byENERGY) {
		## Select the food group energy
		contentNRG  <- content[content$tag == "ENERC_KCAL",]
		## Merge and divide the total energy [(kcal/capita)/day] by the energy per food group (kcal) 
		## to get [(g/capita}/day]
		TWeight  <- merge(contentNRG[,c("code", "value")], consumption, by = "code")
		TWeight$Mass  <- TWeight[,"FdGp1_Val"] / TWeight[,"MNutr_Val"]

    ## add mass to the content
		content  <- merge(content[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutr_Val")], TWeight[,c("Code_FdGp1", "Mass")], by = "Code_FdGp1", all.x = TRUE)
		
	} else { # if (use == "WEIGHT"){
		## Merge and calculate the weight per food group (g/capita/day)
		TWeight  <- merge(consumption, Wg, by = "Code_FdGp1")
		TWeight$Mass  <- TWeight[,"FdGp1_Val"] * TWeight[,"Edible"] * TWeight[,"Yield"]

		## add mass to the content
		content  <- merge(content, TWeight[,c("Code_FdGp1", "Mass")], by = "Code_FdGp1", all.x = TRUE)
	}

  ## Calculate micronutrient per person per day
	content$MNutr_PersonDay  <-  ((content[,"MNutr_Val"] / 1000) * content[,"Mass"])

  ### PART 2 : Use or not Miller equation to estimate the impact of phytic acid on Zinc.
	if (Miller) {
		## Create variables with Phytate and Zinc values.
		i  <- content[, "Tagname"] == "PHYTAC"
		Phytate  <-  sum(content[i, "MNutr_PersonDay"], na.rm = TRUE)
		i  <- content[, "Tagname"] == "ZN"
		Zinc  <- sum(content[i, "MNutr_PersonDay"], na.rm = TRUE)

		## Calculate mineral fractional absorption for zinc (Miller)
		FAZ  <- (0.5/ Zinc) * ((0.13 + Zinc + 0.1 *(1 + Phytate / 1.2)) -  sqrt((0.13 + Zinc + 0.1 * (1 + Phytate / 1.2))^2 - 4 *(0.13 * Zinc)))
		## Multiply fractional absorption for zinc with the zinc value
		i  <- content[,"Tagname"] == "ZN"
		content[i, "MNutr_PersonDay"]  <- content[i, "MNutr_PersonDay"] * FAZ
	}

	content  <- content[,c("Tagname", "MNutr_PersonDay", "Code_FdGp1", "Item_FdGp1", "MNutr_Val", "Mass")]
	return(content)
}

