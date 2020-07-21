
library(diets)
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
	d$Units <- "permille"

  ## Open the file with ISO3 to country name links.
	isoC <- readRDS("data/countries2.rds")
  
  ## Create a vector with all the countries name in the Fortif table
	d$Country <- trimws(d$Country)
	uc <- unique(d$Country)

  ## Check what merge and merge them
	i <- match(uc, isoC$matchname)
	m <- data.frame(Country=isoC$NAME[i], ISO3 = isoC$ISO3[i], stringsAsFactors=FALSE)

	#m[is.na(m[,2]), ]

	dd <- merge(d, m, by="Country")
	colnames(dd) = c("area", "code", "product", "tag", "desc", "unit", "value", "ISO3") 
	dd <- dd[dd$value > 0, ]
	
	content <- diets::nutrientContent(continent="", redpalmoil=0, orangesweetpot=0)
	
	i <- cbind(id=1:nrow(dd), m=match(dd$code, content$code))
	i <- data.frame(na.omit(i))
	dd$group = ""
	dd$group[i$id] <- content$group[i$m]

	dd <- dd[, c("ISO3", "area", "product", "code", "group", "tag", "unit", "value", "desc") ]
	
    ## Calculate the mass of cooking Oil (all the oils)
    # intakeCA <- intake[intake$tag == "CA", c("code", "mass")]  # or group
    #CkOil <- intakeCA[intakeCA[,"group"] %in%  c(2571, 2572, 2573, 2574, 2575, 2576, 2577, 2578, 2579, 2580, 2581, 2582, 2586),]
	# all oils, except Fish oil

	ckoils <- c('Coconut Oil', 'Cottonseed Oil', 'Groundnut Oil', 'Maize Germ Oil', 'Oilcrops Oil, Other', 'Olive Oil', 'Palm Oil', 'Palmkernel Oil', 'Rape and Mustard Oil', 'Ricebran Oil', 'Sesameseed Oil', 'Soyabean Oil', 'Sunflowerseed Oil')
    #any(ckoils %in% content$group)
	CkOil <- unique(content[content$group %in% ckoils, c("group", "code")])

	i <- which(dd$product == "Cooking Oil")
	x <- dd[i,]	
	y <- lapply(1:nrow(CkOil), function(i) {
		x$code <- CkOil$code[i]
		x$group <- CkOil$group[i]
		x }		
	)
	y <- do.call(rbind, y)

	dd <- rbind(dd[-i,], y)
	
	#dd <- data.frame(ISO3 = dd$ISO3, area=dd$Country, dd[, c("Code_FdGp", "Crop", "Tagname", "Desc", "Units", "Values")])
	#colnames(fort) = c("area", "code", "group", "tag", "desc", "unit", 

	saveRDS(dd, "pkg/fortification.rds")
}
