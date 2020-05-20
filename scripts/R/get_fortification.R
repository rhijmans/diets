
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
   isoC <- readRDS("data/countries2.rds")
  
  ## Create a vector with all the countries name in the Fortif table
  d$Country <- trimws(d$Country)
  d2 <- unique(d$Country)

  ## Check what merge and merge them
  i <- match(d2, isoC$matchname)
  m <- data.frame(Country=d2, ISO3 = isoC[i, "ISO3"], stringsAsFactors=FALSE)

	m[is.na(m[,2]), ]

  dFinal <- merge(d, m, by="Country")

	dFinal <- data.frame(ISO3 = dFinal$ISO3, Area=dFinal$Country, dFinal[, c("Code_FdGp", "Crop", "Tagname", "Desc", "Units", "Values")])

	saveRDS(dFinal, "pkg/fortification.rds")
}
