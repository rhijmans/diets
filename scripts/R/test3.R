
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	dp <- "C:/github/diets/scripts"
} else {
	dp <- "C:/Users/jccaro/diets/"
}

# now with a function by country 
# that is cleaner, and it 
# makes it easy to parellelize, if necessary

country_intake <- function(cons, cont) {
	years <- unique(cons$year)
	yout <- list()
	for (j in 1:length(years)) {
		year_cons <- cons[cons$year == years[j],  c("group", "value")]
		intake <- nutrientIntake(year_cons, content, verbose=FALSE)
		# adjust intake stuff here
		intake <- aggregate(intake[, "intake", drop=FALSE], intake[, c("tag", "unit", "desc")], sum, na.rm=TRUE)
		intake$year <- years[j]
		yout[[j]] <- intake
	}
	do.call(rbind, yout)
}

library(diets)

consumption <- readRDS(file.path(dp, "FBS.rds"))
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("ISO3", "Year", "Item", "Value")]
colnames(consumption) <- c("country", "year", "group", "value")
content <- nutrientContent(continent="", redpalmoil=0.5, orangesweetpot=0.2)
fort <- readRDS(system.file("ex/fortification.rds", package="diets"))
#fcontent <- fortify(content, fort)

countries <- unique(consumption$country)
##for testing
#countries <- countries[1:5]
s <- Sys.time()
out <- list()
for (i in 1:length(countries)){
	print(countries[i]); flush.console()
	country_cons <- consumption[consumption$country == countries[i], ]
	# here some logic to select the content based on the country (continent membership)
	out[[i]] <- data.frame(country=countries[i], country_intake(country_cons, content))
}
e <- Sys.time()
(e - s) / length(countries)


lapply(out[1:4], head)
x <- do.call(rbind, out)
head(x)
tail(x)
