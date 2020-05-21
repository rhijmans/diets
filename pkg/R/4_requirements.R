
## Give population micronutrient requirements by Age group for a population in a year
nutrient_requirements  <- function(pop, reqs){

# This replacement is relevant but should be the 
# responsibility of the caller (to use ZN Physio in stead of ZN)
# and could be done with the output of this function
#	if(zn) {
#		i  <- reqs$Tagname == "ZN"
#		reqs <- reqs[!i,]
#		i  <- reqs$Tagname == "ZN Physio"
#		reqs$Tagname[i] <- "ZN"
#	} else {
#		i  <- reqs$Tagname == "ZN Physio"
#		reqs <- reqs[!i,]
#	}

### PART 1 : Population distribution inside the defined groups of the population requirements.
  ## Initialize population at 0
  

	# deal with the 6-12 months olds?
  ## Population repartition below one year old : 50/50 between 0-6 months and 6-12 months.
#	reqs[, "pop"]  <- ifelse(reqs[, "Sex"] == "Females" & reqs[, "AgeGrpEnd"] == 0.5 | reqs[, "Sex"] == "Females" & reqs[, "AgeGrpEnd"] == 1.0, POP_Y1_F / 2, reqs[, "pop"])
#	reqs[, "pop"]  <- ifelse(reqs[, "Sex"] == "Males" & reqs[, "AgeGrpEnd"] == 0.5 | reqs[, "Sex"] == "Males" & reqs[, "AgeGrpEnd"] == 1.0, POP_Y1_M / 2, reqs[, "pop"])

	i <- which(pop$AgeGrp == 0 )
	r <- pop[c(i,i),]
	r$PopMale <- r$PopMale / 2 
	r$PopFemale <- r$PopFemale / 2
	r[1:2,"AgeGrpSpan"] <- 0.5
	r[2,"AgeGrp"] <- 0.5
	r[2,"AgeGrpStart"] <- 0.5
	pop <- rbind(pop[-i,], r)
	pop$AgeGrpEnd <- pop$AgeGrpStart +  pop$AgeGrpSpan

	u <- unique(cbind(reqs$AgeGrpStart, reqs$AgeGrpEnd))
	u <- u[order(u[,1]), ]
	
	reqs$pop  <- 0
	for (i in 1:nrow(u)){
		## Extract population value by sex and age
		j <- pop$AgeGrp >= u[i,1] & pop$AgeGrp < u[i,2]
		if (sum(j) == 0) stop("what?")
		## Selection the right sex and age in population requirements
		s <- reqs$PhysioStatus == "None" & reqs$AgeGrpStart == u[i,1]
		reqs$pop[s & reqs$Sex == "Males"] <- sum(pop[j, "PopMale"])
		reqs$pop[s & reqs$Sex == "Females"] <- sum(pop[j, "PopFemale"])		
	}

### estimate women's physiological status and babies.
  ## Create variables with the one year old population by gender and both
	POP_Y1  <- pop[pop[,"AgeGrpStart"] < 1, ]
	POP_Y1  <- sum(POP_Y1$PopMale + POP_Y1$PopFemale)


  ## Pregnant woman population (assumed to be equal to 3/4 of first year population)
	p <- reqs$Sex == "Females" & reqs$PhysioStatus == "Pregnancy"
	
  ## 1/4 of the pregnant population for the 2 extreme age groups : 14-19 years and 31-51 years.
	reqs[p, "pop"] <- POP_Y1 * 0.25 * 0.75
  ## Half for the 19 - 31 years.
	reqs[p & reqs$AgeGrpStart == 19, "pop"] <- POP_Y1 * 0.5 * 0.75

  ## Lactating woman population (assumed to be equal of the first year population)
	l  <-  reqs$Sex == "Females" & reqs$PhysioStatus == "Lactation"
  ## 1/4 of the lactating population for the 2 extrem age groups : 14-19 years and 31-51 years.
	reqs[l, "pop"]  <- (POP_Y1 * 0.25)
  ## Half for the 19 - 31 years.
	reqs[l & reqs$AgeGrpStart == 19, "pop"]  <- POP_Y1 * 0.5

	reqs$req <- reqs$pop * reqs$value
	a <- aggregate(reqs[, c("pop", "req"), drop=FALSE], reqs[, "tag", drop=FALSE], sum)
	a$req <- a$req / a$pop
	a$pop <- NULL
	return(a)
	
# RH the below just seemed too weird. 

  ### Add the pregnant/lactating population from their age group.
  ## Select rows for lactating and pregnant population
#	popNotNone <- unique(reqs[reqs[, "PhysioStatus"] != "None" , c( "Sex", "AgeGrpStart", "pop")])
  ## Aggreagte the population
#	popNotNone <- aggregate(data.frame(subst = popNotNone$pop), popNotNone[, c("Sex", "AgeGrpStart")], sum)
#	popNotNone$PhysioStatus = "None"
  ## Merge the new table with population surplus to the old one.
#	ReqPFinal <- merge(reqs, popNotNone, by.x =c("Sex", "AgeGrpStart", "PhysioStatus") , by.y = c("Sex", "AgeGrpStart", "PhysioStatus"), all.x = TRUE)
  ## 0 for male and age group not between 14-51 years old.
#	ReqPFinal[is.na(ReqPFinal[, "subst"]), "subst"] <- 0
  ## Substract the surplus to the population amount.
#	ReqPFinal[, "pop"] <- ReqPFinal[, "pop"] - ReqPFinal[, "subst"]
#	ReqPFinal[, "subst"]  <- NULL
#	return(ReqPFinal)
}
