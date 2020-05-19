

## Change Units of the supplementary Foods Table
.FCT_Sup_data  <- function(){

  ## Name of the RDS file
  fr <- file.path("data/FCTSup_MNutr.rds")
  return(readRDS(fr))
  
  if (!file.exists(fr)) {
 
    ## Open the csv
    NutriWA <- utils::read.csv(file.path(.dataPath, "FCTSup.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

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


.fixUSDA <- function(USDSA_Mnutr) {
  ## Change Units name of Kcal
  i  <- USDA_MNutr$Units == "kcal"
  USDA_MNutr$Tagname[i] <- "ENERC_KCAL"
  i  <- USDA_MNutr$Units == "kj"
  USDA_MNutr$Tagname[i] <- "ENERC_KJ"

  ## Tagnames improvment (http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/)
  i <- USDA_MNutr$Tagname %in% c("F10D0", "F12D0", "F13D0", "F14D0", "F14D1", "F15D0", "F15D1", "F16D0", "F16D1C", "F16D1T", "F16D1", "F17D0", "F17D1", "F18D0", "F18D1C", "F18D1T", "F18D1", "F18D2CLA", "F18D2CN6", "F18D2TT", "F18D2", "F18D3CN3", "F18D3CN6", "F18D3", "F18D4", "F20D0", "F20D1", "F20D2CN6", "F20D3N3", "F20D3N6", "F20D3", "F20D4", "F20D5", "F21D5", "F22D0", "F22D1C", "F22D1T" , "F22D1", "F22D4", "F22D5", "F22D6", "F24D0", "F24D1C" , "F4D0", "F6D0", "F8D0")

  USDA_MNutr$Tagname[i] <- paste0(USDA_MNutr$Tagname[i], 'F')

  USDA_MNutr[USDA_MNutr$Tagname %in% "TOCPHA", 'Tagname'] <- "VITE"

  i  <-  USDA_MNutr$Units == "IU" & USDA_MNutr$Tagname == "VITD"
  USDA_MNutr$Tagname[i] <- paste0(USDA_MNutr$Tagname[i], "_IU")

  i  <-  USDA_MNutr$NutrDesc == "Vitamin B-12, added"
  USDA_MNutr$Tagname[i] <- "VITB12_ADD"

  i  <- USDA_MNutr$NutrDesc == "Vitamin E, added"
  USDA_MNutr$Tagname[i] <- "VITE_ADD"

  i  <- USDA_MNutr$NutrDesc == "18:2 t not further defined"
  USDA_MNutr$Tagname[i] <- "F18D2T_NA"

  i  <- USDA_MNutr$NutrDesc == "18:2 i"
  USDA_MNutr$Tagname[i] <- "F18D2_i"

  i  <- USDA_MNutr$NutrDesc == "18:3i"
  USDA_MNutr$Tagname[i] <- "F18D3_i"

  i  <- USDA_MNutr$NutrDesc == "Adjusted Protein"
  USDA_MNutr$Tagname[i] <- "Add_Prot"

  ## Change description name
  USDA_MNutr$NutrDesc  <- trimws(USDA_MNutr$NutrDesc, "right")

  i  <-  USDA_MNutr$NutrDesc == "Vitamin D (D2 + D3)"
  USDA_MNutr$NutrDesc[i] <- "Vitamin D"

  i  <-  USDA_MNutr$NutrDesc == "Folate, total"
  USDA_MNutr$NutrDesc[i] <- "Folate"

  i  <-  USDA_MNutr$NutrDesc == "Carbohydrate, by difference"
  USDA_MNutr$NutrDesc[i] <- "Carbohydrate"

  i  <- USDA_MNutr$NutrDesc == "Vitamin E (alpha-tocopherol)"
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
.getUSDA  <- function(){

	f <- file.path(.dataPath, "USDA_MNutr.rds")
	return(readRDS(f))
	
	
  ## Named of the save file
  path <- tempdir()
  fr <- file.path(path, "USDA_MNutr.rds")

  if (!file.exists(fr)) {
    if(!exists(path)){
      ## Create a directory if needed
      dir.create(path, showWarnings=FALSE, recursive=TRUE)
    }

    ## URL of ucdavis for download
    FURL = "http://data.biogeo.ucdavis.edu/sources/usda/sr27asc.zip"

    ## Name of the untouch files
    USDAFN <- file.path(path, basename(FURL))

    ## download, unzip and create the folder
    if (!file.exists(USDAFN)) {
      download.file(FURL, USDAFN, mode='wb')
      unzip(USDAFN, junkpaths=TRUE, exdir=dirname(USDAFN))
    }

    ### create one table from all the txt with the good column label (source pdf inside sr27asc)
    ## Open from ASCII
    FD_GROUP <- read.table(file.path(path, "FD_GROUP.txt"), header=FALSE, sep="^", quote="~", fill=TRUE, stringsAsFactors=FALSE, na.strings="")
    FOOD_DES <- read.table(file.path(path, "FOOD_DES.txt"), header=FALSE, sep="^", quote="~", fill=TRUE, stringsAsFactors=FALSE, na.strings="")
    NUT_DATA <- read.table(file.path(path, "NUT_DATA.txt"), header=FALSE, sep="^", quote="~", fill=TRUE, stringsAsFactors=FALSE, na.strings="")
    NUTR_DEF <- read.table(file.path(path, "NUTR_DEF.txt"), header=FALSE, sep="^", quote="~", fill=TRUE, stringsAsFactors=FALSE, na.strings="")

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
    saveRDS(USDA_MNutr, fr)
  }
  readRDS(fr)
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



## Add the 2 FCT in one and merge with the links table (link betwen FBS/FCTSup-ProductionImportExport-USDA)
getFCT  <-  function(fbs) {


	d <- utils::read.csv(file.path(.dataPath, "FCTTitles.csv"), stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
	if (fbs) {
		d  <- d[,c("Code_FAOFBS", "Item_FAOFBS", "PHYTAC", "PCT2_FAO", "Code_FAOPIE", "Item_FAOPIE", "PCT1", "NDB_No", "Shrt_Desc")]
	} else {
		d  <- d[,c("Code_Local", "Item_Local", "PHYTAC", "PCT2_Local", "Code_FAOPIE", "Item_FAOPIE", "PCT1", "NDB_No", "Shrt_Desc")]
	}
	names(d)  <- c("Code_FdGp1", "Item_FdGp1", "PHYTAC", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "PCT2_3", "Code_FdGp3", "Item_FdGp3")


  ## Food groups
  FCT_Titles <- d

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

  ## Merge with the links table
  FCT  <- merge(FCT_Titles, FCT_data, all.x = TRUE)

  ## Clean the table
  FCT  <- FCT[,c("Code_FdGp1", "Item_FdGp1", "PHYTAC", "PCT1_2", "Code_FdGp2", "Item_FdGp2", "PCT2_3", "Code_FdGp3", "Item_FdGp3", "Tagname", "MNutrDesc", "Units_MNutr", "MNutr_Val")]

	f <- ifelse(fbs, "pkg/FCT_FBS.rds", "pkg/FCT.rds")
	saveRDS(FCT, f)
	invisible(FCT)
}
