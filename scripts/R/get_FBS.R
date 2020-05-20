

## Get FBS Data from FAOstat
get_FBS <- function() {

	path="process/FBS"
	if(!exists(path)){
		dir.create(path, FALSE, TRUE)
	}

	## URL from FAOSTAT
	#FURL = "http://faostat3.fao.org/faostat-bulkdownloads/FoodBalanceSheets_E_All_Data_(Norm).zip"
	FURL = "http://fenixservices.fao.org/faostat/static/bulkdownloads/FoodBalanceSheets_E_All_Data_(Normalized).zip"
	## Name of the untouch file	
	FBSfn <- file.path(path, basename(FURL))
  

  ## download and unzip
	if (!file.exists(FBSfn)) {
		download.file(FURL, FBSfn, mode = "wb")
	}
	unzip(FBSfn, junkpaths = TRUE, exdir = "FBS")
	
    ## reshape
    csvf <- gsub(".zip", ".csv", file.path(path, basename(FBSfn)))
    fbs <- read.csv(csvf, stringsAsFactors = FALSE)

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
	
	m <- na.omit(data.frame(Area=popCT, ISO3 = isoC[i, "ISO3"], stringsAsFactors=FALSE))

	fbs2 <- merge(fbs, m, by="Area")
#    fbsFinal <- data.frame(ISO3 = fbsFinal$ISO3, Country=fbsFinal$Country, Year=fbsFinal$Year, fbsFinal[, c("ItemCode", "Item", "ElementCode", "Element", "Unit", "Value")])

	fbs2 <- fbs2[, c("ISO3", "Area", "Year", "Item.Code", "Item", "Element.Code", "Element", "Unit", "Value")]
    saveRDS(fbs2, "FBS.rds")

# example for the package	
	rwafbs = fbs[fbs$Area == "Rwanda", ]
    saveRDS(rwafbs, "pkg/rwa_FBS.rds")
}


