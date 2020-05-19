## Calculate deficiencies for a person.
deficiencies <- function(ReqIntk, CV){

  ### PART 1 : Average requirement value & average intake value.
  Req  <- ReqIntk[,c("Tagname", "Req_MNutr_Val", "Pop", "MNutr_PersonDay")]
  ## Multiply required micronutrient value by population and summed.
  Req$ReqNutr_AgeGroupDay  <- Req[, "Req_MNutr_Val"] * Req[, "Pop"]
  Req <- aggregate(Req[,c("ReqNutr_AgeGroupDay", "Pop"), drop = FALSE], Req[, c("Tagname","MNutr_PersonDay")], sum, na.rm = TRUE)
  Req$AvgMNutrReq_PersonDay <- Req[, "ReqNutr_AgeGroupDay"] / Req[, "Pop"]
  Req  <-  Req[, c("Tagname", "AvgMNutrReq_PersonDay")]

  ## Mean the micronutrient intake by person.
  ReqIntk$Add  <- ReqIntk[, "Tagname"]
  Intk <- aggregate(ReqIntk[,c("MNutr_PersonDay"), drop = FALSE], ReqIntk[,c("Tagname", "Add")], mean, na.rm = TRUE)
  names(Intk)[3]  <- "AvgMNutrIntk_PersonDay"
  Intk$Add  <- NULL

  ## Merge
  Final  <- merge(Intk, Req, by = "Tagname")

  ### PART 2 : Deficiencies calculation.
  ## Merge with coeficient of variation.
  Final  <- merge(Final, CV, all.x = TRUE)

  ## Use a normal repartition if CV under or equal 0.3
  Final$Deficiencies <- 100 * (1 - pnorm(Final$AvgMNutrIntk_PersonDay, Final$AvgMNutrReq_PersonDay, Final$CV * Final$AvgMNutrReq_PersonDay))

  ## Use a log-normal repartition if CV upper 0.3
  Lognorm  <- function(mn, cv, p){
    lm <- log(mn^2 / sqrt((mn * cv)^2 + mn^2))
    ls <- sqrt(log((mn * cv)^2 / mn^2 + 1))
    Lognorm <- 100 * (1 - plnorm(p, lm, ls))
  }

  i <- Final$CV > 0.3 & !is.na(Final$CV)
  Final$Deficiencies[i]  <- Lognorm(Final$AvgMNutrReq_PersonDay[i], Final$CV[i], Final$AvgMNutrIntk_PersonDay[i])

  ### PART 3 : Formatting data
  i  <- Final$Units == "mg" & !is.na(Final$Units)
  Final$AvgMNutrIntk_PersonDay[i]  <- Final$AvgMNutrIntk_PersonDay[i] * 1000
  Final$AvgMNutrReq_PersonDay[i]  <- Final$AvgMNutrReq_PersonDay[i] * 1000

  i  <- Final$Units == "mcg" & !is.na(Final$Units)
  Final$AvgMNutrIntk_PersonDay[i]  <- Final$AvgMNutrIntk_PersonDay[i] * 1000000
  Final$AvgMNutrReq_PersonDay[i]  <- Final$AvgMNutrReq_PersonDay[i] * 1000000

  Final  <- Final[, c("Tagname", "Units", "CV", "AvgMNutrIntk_PersonDay", "AvgMNutrReq_PersonDay", "Deficiencies")]
  names(Final)  <- c("Tagname", "Units", "CV", "AvgMNutr_Intk", "AvgMNutr_Req", "Deficiencies")

  return(Final)
}
