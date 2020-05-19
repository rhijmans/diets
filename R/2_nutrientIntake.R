
## Calculation of the average micronutrient intake per person for a country

nutrientIntake  <-  function(Consumption, Content, byENERGY=TRUE, Miller=TRUE, Wg){


  ### PART 1 : Compute mass and micronutrient by person per day from content and consumption according to the value of use.
	if (byENERGY) {
		## Select the food group energy
		ContentNRG  <- Content[Content$Tagname == "ENERC_KCAL",]
		## Merge and divide the total energy (kcal/capita/day) by the energy per food group (kcal) to get the g/capita/day
		TWeight  <- merge(ContentNRG[,c("Code_FdGp1", "MNutr_Val")], Consumption, by = "Code_FdGp1")
		TWeight$Mass  <- TWeight[,"FdGp1_Val"] / TWeight[,"MNutr_Val"]

    ## add mass to the content
		Content  <- merge(Content[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutr_Val")], TWeight[,c("Code_FdGp1", "Mass")], by = "Code_FdGp1", all.x = TRUE)
		
	} else { # if (use == "WEIGHT"){
		## Merge and calculate the weight per food group (g/capita/day)
		TWeight  <- merge(Consumption, Wg, by = "Code_FdGp1")
		TWeight$Mass  <- TWeight[,"FdGp1_Val"] * TWeight[,"Edible"] * TWeight[,"Yield"]

		## add mass to the content
		Content  <- merge(Content, TWeight[,c("Code_FdGp1", "Mass")], by = "Code_FdGp1", all.x = TRUE)
	}

  ## Calculate micronutrient per person per day
	Content$MNutr_PersonDay  <-  ((Content[,"MNutr_Val"] / 1000) * Content[,"Mass"])

  ### PART 2 : Use or not Miller equation to estimate the impact of phytic acid on Zinc.
	if (Miller) {
		## Create variables with Phytate and Zinc values.
		i  <- Content[, "Tagname"] == "PHYTAC"
		Phytate  <-  sum(Content[i, "MNutr_PersonDay"], na.rm = TRUE)
		i  <- Content[, "Tagname"] == "ZN"
		Zinc  <- sum(Content[i, "MNutr_PersonDay"], na.rm = TRUE)

		## Calculate mineral fractional absorption for zinc (Miller)
		FAZ  <- (0.5/ Zinc) * ((0.13 + Zinc + 0.1 *(1 + Phytate / 1.2)) -  sqrt((0.13 + Zinc + 0.1 * (1 + Phytate / 1.2))^2 - 4 *(0.13 * Zinc)))
		## Multiply fractional absorption for zinc with the zinc value
		i  <- Content[,"Tagname"] == "ZN"
		Content[i, "MNutr_PersonDay"]  <- Content[i, "MNutr_PersonDay"] * FAZ
	}

	Content  <- Content[,c("Tagname", "MNutr_PersonDay", "Code_FdGp1", "Item_FdGp1", "MNutr_Val", "Mass")]
	return(Content)
}

