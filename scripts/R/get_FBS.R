

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
    csvf <- gsub(".zip", ".csv", file.path("FBS", basename(FBSfn)))
    fbs <- read.csv(csvf, stringsAsFactors = FALSE)

    ## Change name to the perfect ones !
    # RH: rather keep the orginal names
	#cn <- colnames(fbs)
	#cn <- gsub("\\.", "", cn)
	#cn <- gsub("Area", "Country", cn)
	#colnames(fbs) <- cn

    ## Open the table of ISO3 codes
    isoC <- readRDS("data/countries.rds")
	
	## the countries in the fbs table
	popCT <- unique(fbs$Area)

    ## Check what merge and merge them
    Verif <- match(popCT, isoC[ ,"NAME_FAO"])
    Correct <- data.frame(Area=popCT, ISO3 = isoC[Verif, "ISO3"], stringsAsFactors=FALSE)

    ## If you need to see what doesn't merge
    #popCT[is.na(Verif)]

    ## Matrix of the ISO3 codes who are not in the fbs file
    add <- matrix(c("Azerbaijan", "AZE", "Bolivia (Plurinational State of)", "BOL", "Cabo Verde", "CPV", "China, Hong Kong SAR", "HKG", "China, Macao SAR", "MAC", "China, Taiwan Province of", "TWN", "Congo", "COG", "Côte d'Ivoire", "CIV", "Czechia", "CZE", "Czechoslovakia", "CSK", "Democratic People's Republic of Korea", "PRK", "Ethiopia PDR", "ETH", "Fiji", "FJI", "Iran (Islamic Republic of)", "IRN", "Lao People's Democratic Republic", "LAO", "Montenegro", "MNE", "Netherlands Antilles (former)", "ANT", "Republic of Korea", "KOR", "Republic of Moldova", "MDA", "Saint Vincent and the Grenadines", "VCT", "Serbia" , "SRB", "Sudan (former)", "SDN", "The former Yugoslav Republic of Macedonia", "MKD", "United Republic of Tanzania", "TZA", "USSR", "SUN", "Venezuela (Bolivarian Republic of)", "VEN", "Yugoslav SFR" , "YUG"), ncol=2, byrow=TRUE)

    ## Check the merge between the matrix and the fbs file
    i <- match(Correct$Area, add[,1])
    Verif <- add[i,2]
    Correct$ISO3[!is.na(i)] <- Verif[!is.na(Verif)]

    ## If you need to see what doesn't merge
    #Correct[is.na(Correct$ISO3), 1]

    ## Keep the non NA rows of the list, merge them with the fbs file, class columns and order rows to the final table
    Correct <- Correct[complete.cases(Correct), ]
    fbsFinal <- merge(fbs, Correct, by="Area")
#    fbsFinal <- data.frame(ISO3 = fbsFinal$ISO3, Country=fbsFinal$Country, Year=fbsFinal$Year, fbsFinal[, c("ItemCode", "Item", "ElementCode", "Element", "Unit", "Value")])

	fbsFinal <- fbsFinal[, c("ISO3", "Area", "Year", "Item.Code", "Item", "Element.Code", "Element", "Unit", "Value")]
    saveRDS(fbsFinal, "FBS.rds")

# example for the package	
	rwafbs = fbs[fbs$Area == "Rwanda", ]
    saveRDS(rwafbs, "pkg/rwa_FBS.rds")
	
}


