## Calculate deficiencies for a person.

.get_norm <- function(intake, req, CV) {
	100 * (1 - pnorm(intake, req, CV * intake))
}

.get_old_norm <- function(intake, req, cv) {
	100 * (1 - pnorm(intake, req, cv * req))
}


.get_log_norm <- function(intake, req, cv){
	lmn <- log(req^2 / sqrt((req * cv)^2 + req^2))
	xls <- sqrt(log((req * cv)^2 / req^2 + 1))
	100 * (1 - plnorm(intake, lmn, xls))
}


deficiencies <- function(intake, requirements, CV, old=FALSE) {

	# Average requirement value & average intake value (should not be needed)

	Reqs <- aggregate(requirements[, "req", drop=FALSE], requirements[, "tag", drop=FALSE], sum, na.rm=TRUE)
	Intk <- aggregate(intake[, "intake", drop=FALSE], intake[, "tag", drop=FALSE], sum, na.rm=TRUE)
	
	m  <- merge(Intk, Reqs, by = "tag")

  ### PART 2 : Deficiencies calculation.
  ## Merge with coefficient of variation.
	m  <- merge(m, CV[, c("tag", "CV")], all.x = TRUE)
	m$CV[is.na(m$CV)] <- 0.3
  ## Use a normal distribution if CV under or equal 0.3
	#m$deficiency <- 100 * (1 - pnorm(m$intake, m$req, m$CV * m$intake))

	if (old) {
		m$deficiency <- .get_old_norm(m$intake, m$req, m$CV)
	} else {
		m$deficiency <- .get_norm(m$intake, m$req, m$CV)
	}
  ## Use a log-normal distribution if CV upper 0.3

	i <- m$CV > 0.3 & !is.na(m$CV)
	m$deficiency[i]  <- .get_log_norm(m$intake[i], m$req[i], m$CV[i])
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
