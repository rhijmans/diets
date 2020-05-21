
## Take Estimated_Average_Requirement.csv and convert units to g (if possible)
getReq <- function() {

	for (rda in c(TRUE, FALSE)) {

		if(rda){
			DRI <- base::readRDS("data/RecommendedDietaryAllowances.rds")
		} else {
			DRI <- base::readRDS("data/EstimatedAverageRequirements.rds")
			## Change units for protein from g/kg to g
			i <- DRI$Tagname == "PROCNT"
			DRI$Req_Nutr_Val[i]  <- DRI$Req_Nutr_Val[i] * 60
		}

		## Change all units to g. Default is mcg.
		x <- DRI$Req_Nutr_Val / 1000000
		i <- DRI$Req_Units == "mg"
		x[i]  <- DRI$Req_Nutr_Val[i] / 1000
		i <- DRI$Req_Units == "g" | DRI$Req_Units == "g per kg"
		x[i]  <- DRI$Req_Nutr_Val[i]
		DRI$Req_Nutr_Val <- x

		## Unit adjustment
		DRI$Req_Units<- "g"

		### Add 6-12 months to half the needs of a 1-3 years old for being partial breastfed.
		colnames(DRI)[colnames(DRI) == "Req_Nutr_Val"] <- "Value"
		## Select 1-3 yDRI old in Correct
		Correct <- DRI[DRI$AgeGrp == "1-3",]
		## Change the name of the group
		Correct$AgeGrp <- "0.5-1"
		## Divide by 2 their value
		Correct$Req_Nutr_Val <- Correct$Value / 2
		## Merge the "fake" 0.5-1 file with the base file
		DRI <- merge(DRI, Correct[, c("Gender", "AgeGrp", "Tagname", "Req_Nutr_Val")], by = c("Gender", "AgeGrp", "Tagname"), all.x	 = TRUE)
		DRI$Req_Nutr_Val[!is.na(DRI$Value)]  <- DRI$Value[!is.na(DRI$Value) ]
		## Change all values to 0 because children are only breastfed. The requirement is in "Lactation women"
		i <- DRI$AgeGrp == "0-0.5"
		DRI$Req_Nutr_Val[i] <- 0

		## Creation of a new row with age-group ending yDRI. 101 if no end.
		DRI$AgeGrpEnd  <- DRI$AgeGrpStart + DRI$AgeGrpSpan
		DRI$AgeGrpEnd  <- ifelse(DRI$AgeGrpSpan == -1, 101, DRI$AgeGrpEnd)

		DRI  <- DRI[,c("Gender", "Status", "AgeGrpStart", "AgeGrpEnd", "Tagname", "Req_Nutr_Val")]
		names(DRI)  <- c("Sex", "PhysioStatus", "AgeGrpStart", "AgeGrpEnd", "tag", "value")

		if(rda){
			saveRDS(DRI, "pkg/reqs_rda.rds")
		} else {
			saveRDS(DRI, "pkg/reqs.rds")	
		}
	}
}


