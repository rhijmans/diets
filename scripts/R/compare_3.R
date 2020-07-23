
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

#Load data and calculate intake
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

old <- read.csv(file.path(dp, "data/2017paper_data.csv"))
old <- old[, c("ISO3", "Year", "Fortification", "Tagname", "Units", "Intake")]
#"Requirements", "Prevalence.of.Inadequate.Intake"
#old <- old[old$unit != "GramsPerMille", ]

i <- 3
x <- country_intake(countries[i,1], countries[i,2], fortify=T, adjust=T)

m <- merge(old, x, by.x=c("ISO3", "Year", "Tagname"), by.y=c("country", "year", "tag"))
m$delta <- 100 * ((m$Intake - m$intake) / m$Intake)

y <- m[m$Year %in% 1961, c("Tagname", "Intake", "intake", "delta")]
y[,2:3] <- round(y[,2:3], 5)
y <- y[rev(order(y[,2])), ]
tags <- y$Tagname

z <- m[,c("Tagname", "Intake", "intake")]
i <- match(z[,1], tags)
cols <- rainbow(nrow(y))
plot(z[,2:3], col=cols[i])
abline(0,1)
legend("topleft", legend=tags[1:4], col=cols[1:4], pch=1)

m$delta <- ((m$Intake - m$intake) / m$Intake)
a <- tapply(m$delta, m$Tagname, mean)
sort(round(a, 3))

country="AFG"; region="Asia"


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

