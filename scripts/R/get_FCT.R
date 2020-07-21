

## Change Units of the supplementary Foods Table
.FCT_Sup_data  <- function(){

	fr <- file.path("data/FCTSup_MNutr.rds")
  
	if (!file.exists(fr)) {
 
		## Open the csv
		NutriWA <- utils::read.csv("data/FCTSup.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

		## Change unit to per Thousand. Default is mcg
		x <- NutriWA$Nutr_Val / 100000
		i <- NutriWA$Units == "mg" | NutriWA$Units == "kJ" | NutriWA$Units == "kcal" | NutriWA$Units == "IU"
		x[i]  <- NutriWA$Nutr_Val[i] / 100
		i <- NutriWA$Units == "g"
		x[i]  <- NutriWA$Nutr_Val[i] * 10
		NutriWA$Nutr_Val <- x

		## Unit adjustment
		NutriWA$Units <- ifelse(NutriWA$Units == "kJ" | NutriWA$Units == "kcal" | NutriWA$Units == "IU", NutriWA$Units, "GramsPerMille")

		saveRDS(NutriWA, fr)
    }

	readRDS(fr)
}


.fixUSDA <- function(USDA_MNutr) {
## Change Units name of Kcal
	i  <- USDA_MNutr$Units == "kcal"
	sum(i)
	USDA_MNutr$Tagname[i] <- "ENERC_KCAL"
	i  <- USDA_MNutr$Units == "kJ"
	sum(i)
	USDA_MNutr$Tagname[i] <- "ENERC_KJ"

## Tagnames improvment (http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/)
	i <- USDA_MNutr$Tagname %in% c("F10D0", "F12D0", "F13D0", "F14D0", "F14D1", "F15D0", "F15D1", "F16D0", "F16D1C", "F16D1T", "F16D1", "F17D0", "F17D1", "F18D0", "F18D1C", "F18D1T", "F18D1", "F18D2CLA", "F18D2CN6", "F18D2TT", "F18D2", "F18D3CN3", "F18D3CN6", "F18D3", "F18D4", "F20D0", "F20D1", "F20D2CN6", "F20D3N3", "F20D3N6", "F20D3", "F20D4", "F20D5", "F21D5", "F22D0", "F22D1C", "F22D1T" , "F22D1", "F22D4", "F22D5", "F22D6", "F24D0", "F24D1C" , "F4D0", "F6D0", "F8D0")

	USDA_MNutr$Tagname[i] <- paste0(USDA_MNutr$Tagname[i], 'F')
	USDA_MNutr[USDA_MNutr$Tagname %in% "TOCPHA", 'Tagname'] <- "VITE"

	i  <-  USDA_MNutr$Units == "IU" & USDA_MNutr$Tagname == "VITD"
	sum(i)
	USDA_MNutr$Tagname[i] <- paste0(USDA_MNutr$Tagname[i], "_IU")

	i  <-  USDA_MNutr$NutrDesc == "Vitamin B-12, added"
	sum(i)
	USDA_MNutr$Tagname[i] <- "VITB12_ADD"

	i  <- USDA_MNutr$NutrDesc == "Vitamin E, added"
	sum(i)
	USDA_MNutr$Tagname[i] <- "VITE_ADD"

	i  <- USDA_MNutr$NutrDesc == "18:2 t not further defined"
	sum(i)
	USDA_MNutr$Tagname[i] <- "F18D2T_NA"

	i  <- USDA_MNutr$NutrDesc == "18:2 i"
	sum(i)
	USDA_MNutr$Tagname[i] <- "F18D2_i"

	i  <- USDA_MNutr$NutrDesc == "18:3i"
	sum(i)
	USDA_MNutr$Tagname[i] <- "F18D3_i"

	i  <- USDA_MNutr$NutrDesc == "Adjusted Protein"
	sum(i)
	USDA_MNutr$Tagname[i] <- "Add_Prot"

  ## Change description name
	#USDA_MNutr$NutrDesc  <- trimws(USDA_MNutr$NutrDesc, "right")
	USDA_MNutr$NutrDesc  <- trimws(USDA_MNutr$NutrDesc)

	i  <-  USDA_MNutr$NutrDesc == "Vitamin D (D2 + D3)"
	sum(i)
	USDA_MNutr$NutrDesc[i] <- "Vitamin D"

	i  <-  USDA_MNutr$NutrDesc == "Folate, total"
	sum(i)
	USDA_MNutr$NutrDesc[i] <- "Folate"

	i  <-  USDA_MNutr$NutrDesc == "Carbohydrate, by difference"
	sum(i)
	USDA_MNutr$NutrDesc[i] <- "Carbohydrate"

	i  <- USDA_MNutr$NutrDesc == "Vitamin E (alpha-tocopherol)"
	sum(i)
	USDA_MNutr$NutrDesc[i] <- "Vitamin E"

  ## Change Units to per Thousand. Default is for mcg
	x <- USDA_MNutr$Nutr_Val / 100000
	i <- USDA_MNutr$Units == "mg" | USDA_MNutr$Units == "kJ" | USDA_MNutr$Units == "kcal" | USDA_MNutr$Units == "IU"
	x[i]  <- USDA_MNutr$Nutr_Val[i] / 100
	i <- USDA_MNutr$Units == "g"
	x[i]  <- USDA_MNutr$Nutr_Val[i] * 10
	USDA_MNutr$Nutr_Val <- x

## Unit adjustment to per thousand
	USDA_MNutr$Units <- ifelse(USDA_MNutr$Units == "kJ" | USDA_MNutr$Units == "kcal" | USDA_MNutr$Units == "IU", USDA_MNutr$Units, "GramsPerMille")

	USDA_MNutr
}


## download and shape USDA SR27 (nutrients database)

# RH updated to SR28
# note that the are data files and update files (add, change, remove)


.getUSDA  <- function(){

	.read_usda <- function(f) {
		read.table(f, header=FALSE, sep="^", quote="~", fill=TRUE, stringsAsFactors=FALSE, na.strings="")
	}


	path <- "process/sr28"
	f <- file.path(path, "USDA_MNutr.rds")

	if (!file.exists(f)) {

		u <- "https://www.ars.usda.gov/ARSUserFiles/80400535/DATA/SR/sr28/dnload/sr28asc.zip"
		fmain <- file.path(path, basename(u))
		if (!file.exists(fmain)) {
			dir.create(dirname(fmain), FALSE, TRUE)
			download.file(u, fmain, exdir=dirname(fmain))
			unzip(fmain, exdir=dirname(fmain))
		}
		u <- "https://www.ars.usda.gov/ARSUserFiles/80400535/DATA/SR/sr28/dnload/sr28upd0516.zip"
		fsup <- file.path(path, basename(u))
		if (!file.exists(fsup)) {
			download.file(u, fsup)
			unzip(fsup, exdir=dirname(fsup))
		}
		### create one table from all the txt with the good column label (source pdf inside sr27asc)
		## Open from ASCII
		FD_GROUP <- .read_usda(file.path(path, "FD_GROUP.txt"))		

		FOOD_DES <- .read_usda(file.path(path, "FOOD_DES.txt"))
		FOOD_add <- .read_usda(file.path(path, "ADD_FOOD.txt"))
		FOOD_DES <- rbind(FOOD_DES, FOOD_add)
		FOOD_chg <- .read_usda(file.path(path, "CHG_FOOD.txt"))
		i <- match(FOOD_chg$V1, FOOD_DES$V1)
		stopifnot(all(!is.na(i)))
		FOOD_DES[i,] <- FOOD_chg

		NUT_DATA <- .read_usda(file.path(path, "NUT_DATA.txt"))
		NUT_del  <- .read_usda(file.path(path, "DEL_NUTR.txt"))
		x <- merge(cbind(NUT_DATA[, 1:2], id=1:nrow(NUT_DATA)), NUT_del)
		NUT_DATA <- NUT_DATA[-x$id, ]

		NUT_add  <- .read_usda(file.path(path, "ADD_NUTR.txt"))
		NUT_DATA <- rbind(NUT_DATA, NUT_add)

		NUT_chg <- .read_usda(file.path(path, "CHG_NUTR.txt"))
		x <- merge(cbind(NUT_DATA[, 1:2], id=1:nrow(NUT_DATA)), NUT_chg)
		NUT_DATA[x$id, ] <- x[, -c(1:3)]

		NUTR_DEF <- .read_usda(file.path(path, "NUTR_DEF.txt"))

		## Column label
		names(FD_GROUP) <- c("FdGrp_Cd", "Fd_Grp_Desc")
		names(FOOD_DES) <- c("NDB_No", "FdGrp_Cd", "Long_Desc", "Shrt_Desc", "ComName", "ManufacName", "Survey", "Ref_desc", "Refuse", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor")
		names(NUT_DATA) <- c("NDB_No", "Nutr_No", "Nutr_Val", "Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd", "Ref_NDB_No", "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB", "Stat_cmt", "AddMod_Date", "CC")
		names(NUTR_DEF) <- c("Nutr_No", "Units", "Tagname", "NutrDesc", "Num_Dec", "SR_Order")

		## All in one file (merge, merge, merge)
		NUTR <- merge(NUTR_DEF, NUT_DATA, by = "Nutr_No")
		FOOD  <- merge(FD_GROUP, FOOD_DES, by = "FdGrp_Cd")
		USDA_MNutr <- merge(FOOD, NUTR, by = "NDB_No")	
		
		USDA_MNutr <- .fixUSDA(USDA_MNutr)

	#	saveRDS(USDA_MNutr, "E:/bitbucket/diets/diets/inst/external/USDA_MNutr.rds")	
		saveRDS(USDA_MNutr, f)
	}
	readRDS(f)
}



##################################### Food composition table #################################################
#
###### Description :
#   This function return content of food groups from different references food composition tables.
#   To merge and weighted the data, 2 sublevels were created : FdGp1 is the primary key lvl. FdGp2 is the interlvl who can be deleted. FdGp3 is the lvl with all the Micronutrient/Nutrient value. Between each of them you can weight the value of the subfood group (PCT1_2, PCT2_3).
# The example data frame is composed as followed :
#   "Code_FdGp1" = Code food group 1
#   "Item_FdGp1" = Description of the food group 1
#   "PHYTAC" = Phytic acide in the food group 1
#   "PCT1_2" = Percentage of the category 2 in the category 1
#   "Code_FdGp2" = Code food group 2
#   "Item_FdGp2" = Description of the food group 2
#   "PCT2_3" = Percentager of the category 3 in the category 2
#   "Code_FdGp3" = Code food group 3
#   "Item_FdGp3" = Description of the food group 3
#   "Tagname" = INFOOD Food Composent Identifier
#   "MNutrDesc" = Description of the micronutrient or nutrient
#   "Units_MNutr" = Units of the micronutrient or nutrient
#   "MNutr_Val" = Value of the micronutrient or nutrient
#
#
###### References :
#
#   US Department of Agriculture, Agricultural Research Service, Nutrient Data Laboratory (2015) USDA National Nutrient Database for Standard Reference, Release 27 (slightly revised). Version Current:  May 2015.
#   FAO (2012) West African Food Composition Table. Rome.
#   University of California at Berkeley International Minilist. WorldFood Dietary Assessment System, 2nd edition.
#   Wessells KR, Singh GM, Brown KH (2012) Estimating the Global Prevalence of Inadequate Zinc Intake from National Food Balance Sheets: Effects of Methodological Assumptions.


#faoi <- read.csv("data/FAOSTAT_FBS_items.csv")
#faog <- read.csv("data/FAOSTAT_FBS_itemgroups.csv")


sb2728_change <- function() {
m <- cbind(
old=c('Babyfood, cereal, mixed, dry', 'Beef, New Zealand, imported, kidney, cooked, boiled', 'Beef, New Zealand, imported, liver, cooked, boiled', 'Beef, New Zealand, imported, tongue, cooked, boiled', 'Beef, New Zealand, imported, tripe uncooked, cooked, boiled', 'Beverages, Green tea, brewed, from bags', 'Cocoa mix, powder', 'Coffee, brewed from grounds, prepared with tap water', 'Coffee, instant, regular, powder', 'Crustaceans, shrimp, mixed species, cooked, moist heat', 'Crustaceans, shrimp, untreated, cooked', 'Fish, cod, Pacific, cooked, dry heat', 'Fish, cod, Pacific, untreated, cooked', 'Fish, dolphinfish, cooked, dry heat', 'Fish, pollock, Alaska, cooked, dry heat', 'Fish, pollock, Alaska, untreated, cooked', 'Fish, salmon, sockeye, untreated, cooked', 'Hibiscus tea', 'Kiwifruit, gold, raw', 'Macaroni, cooked, unenriched', 'OSCAR MAYER, Pork Sausage Links (cooked)', 'Pasta, corn, cooked', 'Peaches, raw', 'Rice, white, glutinous, cooked', 'Rice, white, long-grain, regular, cooked, unenriched, without salt', 'SPICES,MUSTARD SD,GROUND', 'Tea, black, brewed, prepared with tap water', 'Tea, herb, other than chamomile, brewed'),  

new=c('Babyfood, cereal, mixed, dry fortified', 'Beef, New Zealand, imported, variety meats and by-products, kidney, cooked, boiled', 'Beef, New Zealand, imported, variety meats and by-products liver, cooked, boiled', 'Beef, New Zealand, imported, variety meats and by-products, tongue, cooked, boiled', 'Beef, New Zealand, imported, variety meats and by-products, tripe uncooked, raw', 'Beverages, tea, green, brewed, regular', 'Cocoa mix, NESTLE, Rich Chocolate Hot Cocoa Mix', 'Beverages, coffee, brewed, prepared with tap water', 'Beverages, coffee, instant, regular, powder', 'Crustaceans, shrimp, mixed species, cooked, moist heat (may have been previously frozen)', 'Crustaceans, shrimp, cooked (not previously frozen)', 'Fish, cod, Pacific, cooked, dry heat (may have been previously frozen)', 'Fish, cod, Pacific, cooked (not previously frozen)', '', 'Fish, pollock, Alaska, cooked, dry heat (may have been previously frozen)', 'Fish, pollock, Alaska, cooked (not previously frozen)', 'Fish, salmon, sockeye, cooked, dry heat', 'Beverages, tea, hibiscus, brewed', 'Kiwifruit, ZESPRI SunGold, raw', 'Pasta, cooked, unenriched, with added salt', 'Pork sausage, link/patty, cooked, pan-fried', "Pasta, gluten-free, corn, cooked", 'Peaches, yellow, raw', 'Rice, white, glutinous, unenriched, cooked', 'Spices, mustard seed, ground', 'Spices, mustard seed, ground', 'Beverages, tea, black, brewed, prepared with tap water', 'Beverages, tea, herb, other than chamomile, brewed'))

m[m[,2] != "", ]
}


## Add the 2 FCT in one and merge with the links table (link betwen FBS/FCTSup-ProductionImportExport-USDA)
getFCT  <-  function() {


		# this table provides the link between USDA food groups (that have the micronutrients)
		# and FAO FBS (from where we get the quantities)
		d <- read.csv("data/FCTTitles.csv")
		d  <- d[,c("Code_FAOFBS", "Item_FAOFBS", "PHYTAC", "PCT2_FAO", "Code_FAOPIE", "Item_FAOPIE", "PCT1", "NDB_No", "Shrt_Desc")]
		#} else {
		#	d  <- d[,c("Code_Local", "Item_Local", "PHYTAC", "PCT2_Local", "Code_FAOPIE", "Item_FAOPIE", "PCT1", "NDB_No", "Shrt_Desc")]
		#}
		names(d)  <- c("Code_FdGp1", "Item_FdGp1", "PHYTAC", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "PCT2_3", "Code_FdGp3", "Item_FdGp3")

		f28 <- sb2728_change()
		i <- match(d$Item_FdGp3, f28[,1])
		i <- na.omit(cbind(d=1:length(i), f28=i))
		d$Item_FdGp3[i[,1]] <- f28[i[,2],2]

		# 470 NDB_No codes
		# 270 FAO_PIE codes
		# 97  Code_FAOFBS codes
		# 134 Code_Local

		## Food composition tables : Principal and Supplementary
		FCTP <- .getUSDA()
		FCTS <- .FCT_Sup_data()

		## Select only the needed columns for bind / merge
		FCTP  <- FCTP[,c("NDB_No", "Long_Desc", "Tagname", "NutrDesc", "Units", "Nutr_Val")]
		names(FCTP) <- c("Code_FdGp3", "Item_FdGp3", "Tagname", "MNutrDesc", "Units_MNutr", "MNutr_Val")
		FCTS  <- FCTS[,c("WA_NDB_No", "WA_Shrt_Desc", "Tagname", "NutrDesc", "Units", "Nutr_Val")]
		names(FCTS) <-c("Code_FdGp3", "Item_FdGp3", "Tagname", "MNutrDesc", "Units_MNutr", "MNutr_Val")
		## Bind the two FCT to create the Default FCT
		FCT_data  <- rbind(FCTP, FCTS)


		## Merge with the FCT with the links table
		d$Code_FdGp3 = NULL # 
		FCT  <- merge(d, FCT_data, all.x = TRUE, by=c("Item_FdGp3"))

		## Clean the table
		FCT  <- FCT[,c("Code_FdGp1", "Item_FdGp1", "PHYTAC", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "PCT2_3", "Code_FdGp3", "Item_FdGp3", "Tagname", "MNutrDesc", "Units_MNutr", "MNutr_Val")]
		# removing three non food items
		FCT <- FCT[!is.na(FCT$PCT2_3 ), ]
				
		i <- is.na(FCT$MNutrDesc) & is.na(FCT$Tagname)
		bad <- FCT[i, ]

		FCT <- FCT[!i, ]
		
		# NA tag (sodium) was read as <NA> (missing data)
		FCT[FCT$MNutrDesc=="Sodium, Na", "Tagname"] <- "NA"  

		f <- "pkg/FCT.rds"
		saveRDS(FCT, f)
	#}	
}

