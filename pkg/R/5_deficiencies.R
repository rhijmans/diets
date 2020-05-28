## Calculate deficiencies for a person.
deficiencies <- function(intake, requirements, CV) {

	# Average requirement value & average intake value (should not be needed)

	Reqs <- aggregate(requirements[, "req", drop=FALSE], requirements[, "tag", drop=FALSE], sum)
	Intk <- aggregate(intake[, "intake", drop=FALSE], intake[, "tag", drop=FALSE], sum)
	
	m  <- merge(Intk, Reqs, by = "tag")

  ### PART 2 : Deficiencies calculation.
  ## Merge with coefficient of variation.
	m  <- merge(m, CV[, c("tag", "CV")], all.x = TRUE)
	m$CV[is.na(m$CV)] <- 0.3
  ## Use a normal repartition if CV under or equal 0.3
	m$deficiency <- 100 * (1 - pnorm(m$intake, m$req, m$CV * m$intake))

  ## Use a log-normal repartition if CV upper 0.3
	log_norm  <- function(mn, cv, p){
		lmn <- log(mn^2 / sqrt((mn * cv)^2 + mn^2))
		xls <- sqrt(log((mn * cv)^2 / mn^2 + 1))
		100 * (1 - plnorm(p, lmn, xls))
	}

	i <- m$CV > 0.3 & !is.na(m$CV)
	m$deficiency[i]  <- log_norm(m$req[i], m$CV[i], m$intake[i])
	m
	
  ### PART 3 : Formatting data
  #i  <- m$Units == "mg" & !is.na(m$Units)
  #m$AvgMNutrIntk_PersonDay[i]  <- m$AvgMNutrIntk_PersonDay[i] * 1000
  #m$AvgMNutrReq_PersonDay[i]  <- m$AvgMNutrReq_PersonDay[i] * 1000

  #i  <- m$Units == "mcg" & !is.na(m$Units)
  #m$AvgMNutrIntk_PersonDay[i]  <- m$AvgMNutrIntk_PersonDay[i] * 1000000
  #m$AvgMNutrReq_PersonDay[i]  <- m$AvgMNutrReq_PersonDay[i] * 1000000


  #Final  <- Final[, c("Tagname", "Units", "CV", "AvgMNutrIntk_PersonDay", "AvgMNutrReq_PersonDay", "Deficiencies")]
  #names(Final)  <- c("Tagname", "Units", "CV", "AvgMNutr_Intk", "AvgMNutr_Req", "Deficiencies")

  #return(Final)
}
