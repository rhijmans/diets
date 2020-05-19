
.dataPath <- function() {system.file("external", package="diets")}
s
## Open a file with ISO3 codes and the different countries group (subregion...)
.cgroup <- function() {
	readRDS(file.path(.dataPath(), "GpCntries.rds"))
}

nutrientContent <- function(continent="", redpalmoil=0.5, orangesweetpot=0, fbs=FALSE) {

	continent <- trimws(continent)
	stopifnot(continent %in% c("Africa", "Americas", "Asia", "Europe", "Oceania", ""))
	stopifnot(redpalmoil <= 1)
	stopifnot(orangesweetpot <= 1)

	FCT  <- readRDS(file.path(.dataPath(), ifelse(fbs, "FCT.rds", "FCT_FBS.rds")))

	if (continent != "") {
		## Open a file with continental differencies in fruit and vegetable consumption
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

	i <- is.na(FCT$PCT1_2)
	FCT$PCT1_2[i]  <- "nop"

	OneMerge  <- FCT[FCT$PCT1_2 == "nop",]
  ## FISH have no PCT1_2 so they are directly multiply by PCT2_3 (Select, Multiply and aggregate by PCT2_3)
  ## PCT2_3
	OneMerge$MNutr_Val  <- (OneMerge$MNutr_Val * OneMerge$PCT2_3) / 100
	ShrtFCT  <- aggregate(OneMerge[, c("MNutr_Val"), drop = FALSE], OneMerge[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)


	TwoMerge  <- FCT[FCT$PCT1_2 != "nop",]
	if(nrow(TwoMerge) > 0){
    ## The rest of the food must be divide by PCT2_3 and PCT1_2 (Select, Multiply and aggregate by PCT2_3 and PCT1_2)
    ## PCT2_3
		TwoMerge$MNutr_Val  <- (TwoMerge$MNutr_Val * TwoMerge$PCT2_3) /100
		TwoMerge  <- aggregate(TwoMerge[, c("MNutr_Val"), drop = FALSE], TwoMerge[,c("Code_FdGp1", "Item_FdGp1", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)
		## PCT1_2
		TwoMerge$MNutr_Val  <- (TwoMerge$MNutr_Val * as.double(TwoMerge$PCT1_2)) / 100
		TwoMerge  <- aggregate(TwoMerge[, c("MNutr_Val"), drop = FALSE], TwoMerge[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutrDesc", "Units_MNutr")], sum, na.rm = TRUE)

		## Bind the two group together
		ShrtFCT  <- rbind(ShrtFCT,TwoMerge)
	}

	### Add phytate as a micronutrient.
	PHY  <- FCT[, c("Code_FdGp1", "Item_FdGp1", "PHYTAC")]
	names(PHY) [3]  <- "MNutr_Val"
	## From mg/100g to PerThousand
	PHY$MNutr_Val  <- PHY$MNutr_Val / 100
	PHY  <- unique(data.frame(PHY, Tagname = "PHYTAC", Units_MNutr = "PerThousand", MNutrDesc = "Phytate"))

	## Bind with Phytate
	ShrtFCT  <- rbind(ShrtFCT, PHY)
	ShrtFCT  <- ShrtFCT[,c("Code_FdGp1", "Item_FdGp1", "Tagname", "MNutr_Val", "Units_MNutr", "MNutrDesc")]
	colnames(ShrtFCT) <- c("code", "group", "tag", "value", "unit", "desc")
	return(ShrtFCT)
}

