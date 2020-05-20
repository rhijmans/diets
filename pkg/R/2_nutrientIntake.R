
## Calculation of the average micronutrient intake per person for a country

# compute mass and micro-nutrient by person per day from content and consumption
# by energy or by weight

nutrientIntake  <-  function(consumption, content, weight=NULL, verbose=TRUE){
	
	#stopifnot(c("code_FdGp1", "Item_FdGp1", "tag", "MNutr_Val") %in% colnames(content))
	#stopifnot(c("Code_FdGp1") %in% colnames(consumption))
	stopifnot(c("group", "value") %in% colnames(consumption))
	stopifnot(c("group", "tag", "value") %in% colnames(content))

	if (verbose) {
		i <- which(is.na(match(consumption$group, content$group)))
		cat("consumption groups not found in content:\n")
		print(consumption$group[i]); flush.console()
	}

	if (is.null(weight)) {
		## Select the food group energy
		contentNRG  <- content[content$tag == "ENERC_KCAL",]
		if (NROW(contentNRG) == 0) {
			stop("no records in content$tag with value \"ENERC_KCAL\"")
		}
		## Merge and divide the total energy [(kcal/capita)/day] by the energy per food group (kcal) 
		## RH: the listed unit is indeed kcal, but presumably it should be kcal/g  !?!?
		## to get [(g/capita}/day]
		#TWeight  <- merge(contentNRG[,c("code", "value")], consumption, by = "code")
		#TWeight$Mass  <- TWeight[,"FdGp1_Val"] / TWeight[,"MNutr_Val"]
		
		m  <- merge(consumption, content, by="group")
		m$mass  <- m$value.x / m$value.y
		m$mass[!is.finite(m$mass)] <- 0  # division by 0, or 0/0

    ## add mass to the content
		content  <- merge(content, m[,c("group", "tag", "mass")], by = c("group", "tag")) #, all.x = TRUE)
		
	} else { # if (use == "WEIGHT"){
		## Merge and calculate the weight per food group (g/capita/day)
		m  <- merge(consumption, weight, by = "group")
		m$mass  <- m[,"value"] * m[,"edible"] * m[,"yield"]

		## add mass to the content
		content  <- merge(content, m[,c("group", "mass")], by = "group") #, all.x = TRUE)
	}

  ## Calculate micronutrient per person per day
	content$intake <- content$value * content$mass / 1000
	content <- content[content$intake > 0, ]
	#content  <- content[,c("tag", "MNutr_PersonDay", "Code_FdGp1", "Item_FdGp1", "MNutr_Val", "Mass")]
	return(content)
}

