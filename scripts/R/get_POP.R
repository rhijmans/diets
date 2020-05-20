## Get the UN population file


pop_old_countries <- function(d, oldISO3, oldName, newISO3s)  {
    dd  <- d[d$ISO3 %in% newISO3s, ]
    dd$ISO3  <- oldISO3
    dd$Area <- oldName
    aggregate(dd[, c('PopMale', 'PopFemale')], dd[, c("ISO3", "Area", "Year", "AgeGrp", "AgeGrpStart", "AgeGrpSpan")], sum)
}


library(curl)
get_pop <- function() {

url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"

	path="process/pop"
	if(!exists(path)){
		dir.create(path, FALSE, TRUE)
	}

  	f <- gsub( ".csv", ".rds", file.path(path, basename(url)))
  	if (!file.exists(f)) {
		pop <- read.csv(curl(url), stringsAsFactors=FALSE)
		saveRDS(pop, f)
  	} 
	
	pop <- readRDS(f)
	v <- c("PopMale", "PopFemale", "PopTotal")
	pop[, v] <- pop[, v] * 1000

    ## Open a list of ISO3 codes for all the countries
    isoC <- readRDS("data/countries2.rds")
    popCN <- unique(pop$Location)

  	## Check countries who merge between the iso3 file and the pop file and merge them
  	i <- match(popCN, isoC$matchname)
  	m <- data.frame(Location = popCN, ISO3 = isoC[i, "ISO3"], stringsAsFactors = FALSE)

  	## If you need to see what doesn't merge
  	#popCN[is.na(i)]

  	## Keep the non NA rows of the list, merge them with the UN file, class columns and order rows to the final table
  	m <- m[!is.na(m$ISO3), ]
  	popFinale <- merge(pop, Correct, by = "Location")

  	popFinal <- data.frame(ISO3 = popFinale$ISO3, Area = popFinale$Location, Year = popFinale$Time, popFinale[, c('PopMale', 'PopFemale', 'AgeGrp', 'AgeGrpStart', 'AgeGrpSpan')], stringsAsFactors = FALSE)
  	pop <- popFinal[order(popFinal[, 1], popFinal[, 3], popFinal[,6]), ]

	# create old countries
	x1 <- pop_old_countries(pop, "CSK",  "Czechoslovakia", c("CZE", "SVK"))
	x2 <- pop_old_countries(pop, "SUN",  "Soviet Union", c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB"))

	x3 <- pop_old_countries(pop, "YUG",  "Yugoslavia", c("BIH", "MNE", "HRV", "MKD", "SVN", "SRB", "RKS")) 

# check what FBS is doing (old or new ETH and SDN
	pop$ISO3[pop$ISO3=="ETH"] <- "ETHnew"
	pop$ISO3[pop$ISO3=="SDN"] <- "SDNnew"
	x4 <- pop_old_countries(pop, "ETH",  "Ethiopia", c("ETHnew", "ERI"))
	x5 <- pop_old_countries(pop, "SDN",  "Sudan", c("SDNnew", "SSD"))
	x <- rbind(x1,x2,x3,x4,x5)

	pop <- rbind(pop, x)

	saveRDS(pop, basename(f))
	rwa <- pop[pop$ISO3 == "RWA", ]
	saveRDS(rwa, "pkg/rwa_pop.rds")
}


