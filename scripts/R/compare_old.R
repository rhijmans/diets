this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") { dp <- "C:/github/diets/scripts"
} else { dp <- "C:/Users/jccaro/diets/scripts"}


library(diets)

##################
## compute intake for one country
##################

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
			intake <- adjust_Fe(intake, Nheme)
			intake <- adjust_Zn(intake)
		}
		intake <- aggregate(intake[, "intake", drop=FALSE], intake[, c("tag", "unit", "desc")], sum, na.rm=TRUE)
		intake$year <- years[j]
		yout[[j]] <- intake
	}
	cntr_intake <- do.call(rbind, yout)
	data.frame(country=country, region=region, cntr_intake)		
}

##################
## Load data for all countries
##################
pop <- readRDS(file.path(dp, "WPP2019_PopulationBySingleAgeSex_1950-2019.rds"))
consumption <- readRDS(file.path(dp, "FBS.rds"))
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("ISO3", "Year", "Item", "Value")]
colnames(consumption) <- c("country", "year", "group", "value")
country_reg <- readRDS(file.path(dp, "data/countries.rds"))[, c("ISO3","CONTINENT")]
colnames(country_reg)[2] <- "continent"
Nheme  <- readRDS(system.file("ex", "NHemeIron.rds", package="diets"))
fort <- readRDS(system.file("ex/fortification.rds", package="diets"))
conts <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
content <- lapply(conts, function(i) nutrientContent(continent=i, redpalmoil=0.5, orangesweetpot=0.2))
names(content) <- conts 
countries <- unique(consumption$country)
countries <- merge(data.frame(ISO3=countries), country_reg, by="ISO3")

### old results for comparison
old <- read.csv(file.path(dp, "data/2017paper_data.csv"))
old <- old[, c("ISO3", "Year", "Fortification", "Tagname", "Units", "Intake")]


### pick your country from "countries"
i <- 1
x <- country_intake(countries[i,1], countries[i,2], fortify=T, adjust=T)

m <- merge(old, x, by.x=c("ISO3", "Year", "Tagname"), by.y=c("country", "year", "tag"))
# difference as %
m$delta <- 100 * ((m$Intake - m$intake) / m$Intake)

# pick a year
year <- 1961 
y <- m[m$Year %in% year, c("Tagname", "Intake", "intake", "delta")]
y[,2:3] <- round(y[,2:3], 5)
y <- y[rev(order(y[,2])), ]
y

# plot all years
tags <- y$Tagname
z <- m[,c("Tagname", "Intake", "intake")]
i <- match(z[,1], tags)
cols <- rainbow(nrow(y))
plot(z[,2:3], col=cols[i])
abline(0,1)
legend("topleft", legend=tags[1:4], col=cols[1:4], pch=1)

# average differences over all years.
a <- tapply(m$delta, m$Tagname, mean)
sort(round(a, 3))
