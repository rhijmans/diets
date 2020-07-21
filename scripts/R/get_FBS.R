

## Get FBS Data from FAOstat
get_FBS <- function() {

	path="process/FBS"
	if(!exists(path)){
		dir.create(path, FALSE, TRUE)
	}

	for (method in c("old", "new")) {

		if (method=="old") {
			FURL = "http://fenixservices.fao.org/faostat/static/bulkdownloads/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip"
		} else {
			FURL = "http://fenixservices.fao.org/faostat/static/bulkdownloads/FoodBalanceSheets_E_All_Data_(Normalized).zip"
		}
		
	## Name of the untouch file	
		FBSfn <- file.path(path, basename(FURL))

	  ## download and unzip
		if (!file.exists(FBSfn)) {
			download.file(FURL, FBSfn, mode = "wb")
			unzip(FBSfn, junkpaths = TRUE, exdir =path)
		}
	
		## reshape
		csvf <- gsub(".zip", ".csv", FBSfn)
		fbs <- read.csv(csvf, stringsAsFactors = FALSE)

# there are duplicates (old)
#f = fbs[fbs$Area.Code == 2 & fbs$Year==1961 & fbs$Element=="Food supply (kcal/capita/day)", ]
#f[f$Item == "Eggs", ]
# so we will do a rather costly "unique" below 
      
		## Change name to the perfect ones !
		# RH: rather keep the orginal names
		#cn <- colnames(fbs)
		#cn <- gsub("\\.", "", cn)
		#cn <- gsub("Area", "Country", cn)
		#colnames(fbs) <- cn

		## Open the table of ISO3 codes
		isoC <- readRDS("data/countries2.rds")
		
		## the countries in the fbs table
		ctr <- unique(fbs$Area)

		## Check what merge and merge them
		i <- match(ctr, isoC$matchname)
		## If you need to see what doesn't merge
		#unique(ctr[is.na(i)])
		
		m <- na.omit(data.frame(Area=ctr, ISO3 = isoC[i, "ISO3"], stringsAsFactors=FALSE))

		fbs2 <- merge(fbs, m, by="Area")
	#    fbsFinal <- data.frame(ISO3 = fbsFinal$ISO3, Country=fbsFinal$Country, Year=fbsFinal$Year, fbsFinal[, c("ItemCode", "Item", "ElementCode", "Element", "Unit", "Value")])

		fbs2 <- fbs2[, c("ISO3", "Area", "Year", "Item", "Element", "Unit", "Value")]
# remove duplicates
		fbs2 <- unique(fbs2)

	# example for the package	
		if (method=="new") {
			rwafbs = fbs2[fbs2$Area == "Rwanda" & fbs2$Year == 2017 & fbs2$Element == "Food supply (kcal/capita/day)", ]
			rwafbs$ISO3 = NULL
			saveRDS(rwafbs, "pkg/rwa_FBS.rds")
		} else {
			fbsold = fbs2
		}
	}
	
	x <- rbind(fbsold, fbs2)
	# sorting takes a while ;(
	x <- x[order(x$Area, x$Year, x$Item, x$Element), ]
	saveRDS(x, "FBS.rds")
}

