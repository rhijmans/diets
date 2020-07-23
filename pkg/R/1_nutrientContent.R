

## Open a file with ISO3 codes and the different countries group (subregion...)
#.cgroup <- function() {
#	readRDS(file.path(.dataPath(), "GpCntries.rds"))
#}

# consider make "continent" a data.frame that the user provides
# so that it could be more granular (e.g. country)

nutrientContent <- function(continent="", redpalmoil=0.5, orangesweetpot=0) {

	.dataPath <- function() {system.file("ex", package="diets")}

	continent <- trimws(continent)
	stopifnot(continent %in% c("Africa", "Americas", "Asia", "Europe", "Oceania", ""))
	stopifnot(redpalmoil >= 0 & redpalmoil <= 1)
	stopifnot(orangesweetpot >= 0 & orangesweetpot <= 1)

	FCT  <- readRDS(file.path(.dataPath(), "FCT.rds"))

	if (continent != "") {
		## Open a file with continental differences in fruit and vegetable consumption
		FCTcnt <- readRDS(file.path(.dataPath(), "ContiConsump.rds"))
		names(FCTcnt)[1:4]  <- c("Code_FdGp2", "Item_FdGp2", "Code_FdGp1", "Item_FdGp1")
		
		FCT  <- merge(FCT, FCTcnt[,c("Code_FdGp2", "Item_FdGp2", continent)], all.x = TRUE)
		i  <- !is.na(FCT[, continent])
		FCT[i, "PCT1_2"]  <- FCT[i, continent]

		FCT  <- FCT[,c("Code_FdGp1", "Item_FdGp1", "PHYTAC", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "PCT2_3", "Code_FdGp3", "Item_FdGp3", "Tagname", "MNutrDesc", "Units_MNutr", "MNutr_Val")]
	}

	if (redpalmoil > 0) {
		i  <- FCT$Code_FdGp2 == 257
		FCT$PCT2_3[i]  <- 100 * redpalmoil
	}

	if (orangesweetpot > 0){
		i  <- ((FCT$Code_FdGp3 == "11508") | (FCT$Code_FdGp3 == "11510"))
		FCT$PCT2_3[i]  <- 50 * orangesweetpot
		
		i  <- (FCT$Code_FdGp3 == "02_012") | (FCT$Code_FdGp3 == "02_014")
		FCT$PCT2_3[i]  <- 4 * (1-orangesweetpot)
		i  <- FCT$Code_FdGp3 == "02_023"
		FCT$PCT2_3[i]  <- 92 * (1-orangesweetpot)
	}


  ## FISH have no PCT1_2 so they are directly multiply by PCT2_3 (Select, Multiply and aggregate by PCT2_3)
  ## PCT2_3
	i <- is.na(FCT$PCT1_2)
	fish  <- FCT[i, ]

    # % do not add up to 100	
#	fu <- fish[fish$Tagname=="FE", c("Item_FdGp1", "PCT2_3", "MNutr_Val")]
#	fa <- aggregate(fu[, "PCT2_3", drop=FALSE], list(fu$Item_FdGp1), sum)	
#	fa

    # then this is wrong	
#	fish$MNutr_Val  <- fish$MNutr_Val * fish$PCT2_3 / 100
#	ShrtFCT  <- aggregate(fish[, "MNutr_Val", drop = FALSE], fish[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)

    # and this is better
	fish$MNutr_Val  <- fish$MNutr_Val * fish$PCT2_3
	fish <- aggregate(fish[, c("MNutr_Val", "PCT2_3")], fish[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)
	fish$MNutr_Val <- fish$MNutr_Val / fish$PCT2_3
	fish$PCT2_3 <- NULL

## The rest of the food must be divided with PCT2_3 and PCT1_2 (select, multiply and aggregate by PCT2_3 and PCT1_2)
	notf  <- FCT[!i, ]
    ## PCT2_3
	
    # again % do not all add up to 100	
	#nu <- notf[notf$Tagname=="FE", c("Item_FdGp1", "PCT2_3", "Item_FdGp2", "PCT1_2", "MNutr_Val")]
	#na <- aggregate(nu[, "PCT2_3", drop=FALSE], nu[,c("Item_FdGp1", "Item_FdGp2"), drop=FALSE], sum)	
	#na <- na[order(na$Item_FdGp1), ]

# then this is wrong	
	#notf$MNutr_Val  <- (notf$MNutr_Val * notf$PCT2_3) /100
	#notf  <- aggregate(notf[, "MNutr_Val", drop = FALSE], notf[,c("Code_FdGp1", "Item_FdGp1", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)

# and this is better
	notf$MNutr_Val  <- notf$MNutr_Val * notf$PCT2_3
	notf  <- aggregate(notf[, c("MNutr_Val", "PCT2_3")], notf[,c("Code_FdGp1", "Item_FdGp1", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)
	notf$MNutr_Val <- notf$MNutr_Val / notf$PCT2_3

	## PCT1_2
    # again % do not all add up to 100	
	#nu2 <- notf[notf$Tagname=="FE", c("Item_FdGp1", "PCT2_3", "Item_FdGp2", "PCT1_2", "MNutr_Val")]
	#na2 <- aggregate(nu2[, "PCT1_2", drop=FALSE], nu2[, "Item_FdGp1", drop=FALSE], sum)	
	#na2 <- na2[order(na2$Item_FdGp1), ]
	
	#notf$MNutr_Val  <- (notf$MNutr_Val * as.double(notf$PCT1_2)) / 100
	#notf  <- aggregate(notf[, c("MNutr_Val"), drop = FALSE], notf[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)

	# better
	notf$MNutr_Val  <- notf$MNutr_Val * notf$PCT1_2
	notf  <- aggregate(notf[, c("MNutr_Val", "PCT1_2"), drop = FALSE], notf[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)
	notf$MNutr_Val <- notf$MNutr_Val / notf$PCT1_2
	notf$PCT1_2 <- NULL


	## combine fish and not fish
	fct  <- rbind(fish, notf)

	### Add phytate as a micronutrient.
	PHY  <- FCT[, c("Code_FdGp1", "Item_FdGp1", "PHYTAC")]
	names(PHY) [3]  <- "MNutr_Val"
	## From mg/100g to PerThousand
	PHY$MNutr_Val  <- PHY$MNutr_Val / 100
	PHY  <- unique(data.frame(PHY, Tagname = "PHYTAC", Units_MNutr = "PerThousand", MNutrDesc = "Phytate"))

	## Bind with Phytate
	fct  <- rbind(fct, PHY)
	fct  <- fct[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutr_Val", "Units_MNutr", "MNutrDesc")]

# RH
	colnames(fct) <- c("code", "group", "tag", "value", "unit", "desc")
	fct$unit[fct$unit == "GramsPerMille"] <- "permille"
	fct$unit[fct$unit == "PerThousand"] <- "permille"
	fct <- fct[fct$tag != "ENERC_KJ", ] # we use kcal
  
	return(fct)
}


## The fort table should have a "coverage" variable so that 
## we do not assume 100%

fortify <-  function(content, fort) {
	f <- fort[,c("code", "tag", "unit", "value")]
	nr <- NROW(fort)
	if (nr == 0) {	return(content) }
	f <- unique(f)
	if (NROW(f) < nr) {	return(stop("duplicates in fort")) }
	
	colnames(f)[3:4] <- c("funit", "fvalue")
	m <- merge(content, f, by = c("code", "tag"), all.x=TRUE)
	test <- unique(na.omit(m[, c("tag", "unit", "funit")]))
	
	#if (!all(test$unit == test$funit)) {
	#	stop("units do not match")
	#}
	i <- !is.na(m$fvalue)
	m$value[i] <- rowSums(m[i, c("value", "fvalue")])
	m$funit <- NULL
	m$fvalue <- NULL
	return(m)
}



