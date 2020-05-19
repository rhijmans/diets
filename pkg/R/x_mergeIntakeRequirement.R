
## Merge Nutrient Requirements and comsumptions

## trivial, perhaps we do not need a function for that;  

.base_8_ReqIntk <- function(intake, requirements){

  intake$plus <- 1
  ## Aggregate micronutrient intake by nutrient
  IntakeY  <- aggregate(intake[, "MNutr_PersonDay", drop = FALSE], intake[,c("Tagname", "plus")], sum, na.rm = TRUE)
  IntakeY$plus  <- NULL

  ## Merge them to have a table with intake and requirements
  RequirementsIntk  <- merge(requirements, IntakeY, by.x = "Tagname", by.y = "Tagname")

  RequirementsIntk  <- RequirementsIntk[, c("Tagname", "Req_MNutr_Val", "Pop", "MNutr_PersonDay", "Sex", "AgeGrpStart", "PhysioStatus", "AgeGrpEnd")]

  return(RequirementsIntk)
}

