
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	dp <- "C:/github/diets/scripts"
} else {
	dp <- "C:/Users/jccaro/diets/scripts"
}

#install.packages("remotes")
#remotes::install_github("rhijmans/diets/pkg")

library(diets)

# function for a country
##############

#country_intake <- function(cons, cont) {
country_intake <- function(country, region, fortify=TRUE, adjust=TRUE) {

	print(country); flush.console()
	cons <- consumption[consumption$country == country, ]
	cont <- content[[region]]
	if (fortify) {
		fort_country <- fort[fort$area == country, ]
		cont <- fortify(cont, fort_country)
	}
	
	years <- unique(cons$year)
	yout <- list()
	for (j in 1:length(years)) {
		year_cons <- cons[cons$year == years[j],  c("group", "value")]
		intake <- nutrientIntake(year_cons, cont, verbose=FALSE)
		if (adjust) {
			intake <- add_Ca(intake)
			intake <- adjust_Ca(intake)
			intake <- adjust_Zn(intake)
		}
		intake <- aggregate(intake[, "intake", drop=FALSE], intake[, c("tag", "unit", "desc")], sum, na.rm=TRUE)
		intake$year <- years[j]
		yout[[j]] <- intake
	}
	cntr_intake <- do.call(rbind, yout)
	data.frame(country=country, region=region, cntr_intake)		
}

#Load data and calculate intake
##################

pop <- readRDS(file.path(dp, "WPP2019_PopulationBySingleAgeSex_1950-2019.rds"))
consumption <- readRDS(file.path(dp, "FBS.rds"))
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("ISO3", "Year", "Item", "Value")]
colnames(consumption) <- c("country", "year", "group", "value")
country_reg <- readRDS(file.path(dp, "data/countries.rds"))[, c("ISO3","CONTINENT")]
colnames(country_reg)[2] <- "continent"

fort <- readRDS(system.file("ex/fortification.rds", package="diets"))

conts <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
content <- lapply(conts, function(i) nutrientContent(continent=i, redpalmoil=0.5, orangesweetpot=0.2))
names(content) <- conts 
countries <- unique(consumption$country)
countries <- merge(data.frame(ISO3=countries), country_reg, by="ISO3")

old <- read.csv(file.path(dp, "data/2017paper_data.csv"))
old <- old[, c("ISO3", "Year", "Fortification", "Tagname", "Units", "Intake")]
#"Requirements", "Prevalence.of.Inadequate.Intake"
#old <- old[old$unit != "GramsPerMille", ]


x <- country_intake(countries[1,1], countries[1,2], fortify=T, adjust=T)
y <- unique(x[,c("tag", "unit")])
y[y$tag == "CA", ]


m <- merge(old, x, by.x=c("ISO3", "Year", "Tagname"), by.y=c("country", "year", "tag"))

y <- m[m$Year==1961,c("Intake", "intake")]
plot(y)
abline(0,1)

m$delta <- 100 * ((m$Intake - m$intake) / m$Intake)
a <- tapply(m$delta, m$Tagname, mean)



x_old <- merge(x_old, country_names, by="ISO3")

x_new <- x[x$tag %in% c(mn_tag),]
x_new <- x_new[x_new$country %in% c(countries_old),]
x_new <- x_new[x_new$year <= '2011',]
x_old_intake <- x_old[, c("ISO3", "Year", "Tagname", "Intake")]
x_new <- merge(x_new, x_old_intake, by, by.x=c("country", "year", "tag"), by.y=c("ISO3", "Year", "Tagname"))

x_new$diff <- x_new$Intake/x_new$intake

x_new_c <- aggregate(x_new[, "diff", drop=FALSE], x_new[, c("year", "CONTINENT", "tag")], mean, na.rm=TRUE)


#
#mn_tag <- unique(x_old$Tagname)
#countries_old <- unique(x_old$ISO3)

