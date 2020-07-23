
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
		## RH: the listed unit is indeed kcal, but presumably it is kcal/100g
		## see page 39 of manual
		## to get [(g/capita}/day]
		#TWeight  <- merge(contentNRG[,c("code", "value")], consumption, by = "code")
		#TWeight$Mass  <- TWeight[,"FdGp1_Val"] / TWeight[,"MNutr_Val"]
		
		# should this be needed? 
		#contentNRG$value <- contentNRG$value * 100  # to get kcal/g
		# apparently not
		m <- merge(consumption, contentNRG, by="group")
		m$mass  <- m$value.x / m$value.y
		m$mass[!is.finite(m$mass)] <- 0  # division by 0, or 0/0

    ## add mass to the content
		cont  <- merge(content, m[,c("group", "mass")], by = "group") #, all.x = TRUE)
		
	} else { # if (use == "WEIGHT"){
		## Merge and calculate the weight per food group (g/capita/day)
		m  <- merge(consumption, weight, by = "group")
		m$mass  <- m[,"value"] * m[,"edible"] * m[,"yield"]

		## add mass to the cont
		cont  <- merge(content, m[,c("group", "mass")], by = "group") #, all.x = TRUE)
	}

  ## Calculate micronutrient per person per day
	cont$intake <- cont$value * cont$mass / 1000
	#RH if I understand it well
	# I think like this it mg/g * g/1000 = g
	#cont$intake <- cont$value * cont$mass / 1000

	cont <- cont[!is.na(cont$intake), ]
	cont <- cont[cont$intake > 0, ]
	#cont  <- cont[,c("tag", "MNutr_PersonDay", "Code_FdGp1", "Item_FdGp1", "MNutr_Val", "Mass")]
	return(cont)
}

