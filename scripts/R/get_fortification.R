
get_fortif <- function() {
  ## Open the fortification database
  
  d <- utils::read.csv("data/Fortification.csv", stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")

  ## Change Units to per Thousand. Default is for mcg
  x <- d$Values / 1000000
  i <- d$Units == "mg/kg"
  x[i]  <- d$Values[i] / 1000
  i <- d$Units == "IU/kg"
  x[i]  <- d$Values[i] * 0.3 / 1000000
  d$Values <- x
  d$Units <- "GramsPerMille"

  ## Open the file with ISO3 to country name links.
    isoC <- readRDS("data/countries.rds")
  
  ## Create a vector with all the countries name in the Fortif table
  d2 <- unique(d$Country)

  ## Check what merge and merge them
  Verif <- match(d2, isoC[ ,"NAME_FAO"])
  Correct <- data.frame(Country=d2, ISO3 = isoC[Verif, "ISO3"], stringsAsFactors=FALSE)

  ## Matrix of the ISO3 codes who are not in the Fortif file
  add <- matrix(c("Algeria ", "ALG", "Azerbaijan", "AZE", "Bolivia; Plurinational State of", "BOL", "Congo", "COG", "Cote d'Ivoire", "CIV", "Korea; Democratic People", "PRK", "Fiji", "FJI", "Iran; Islamic Republic of", "IRN", "Lao People", "LAO", "Libya", "LIB", "Montenegro" , "MNE", "Netherlands Antilles", "ANT", "Occupied Palestinian Territory", "PSE", "Korea; Republic of", "KOR", "Republic of Moldova", "MDA", "Saint Vincent and the Grenadines", "VCT", "Serbia", "SRB", "Macedonia; former Yugoslav Republic ", "MKD", "Tanzania; United Republic of", "TZA", "Venezuela; Bolivarian Republic of", "VEN"), ncol=2, byrow=TRUE)

  ## Check the merge between the matrix and the Fortif file
  i <- match(Correct$Country, add[,1])
  Verif <- add[i,2]
  Correct$ISO3[!is.na(i)] <- Verif[!is.na(Verif)]

  ## Keep the non NA rows of the list, merge them with the Fortif file, class columns and order rows to the final table
  Correct <- Correct[complete.cases(Correct), ]
  dFinal <- merge(d, Correct, by="Country")

	dFinal <- data.frame(ISO3 = dFinal$ISO3, Area=dFinal$Country, dFinal[, c("Code_FdGp", "Crop", "Tagname", "Desc", "Units", "Values")])

	saveRDS(dFinal, "fortification.rds")
}
