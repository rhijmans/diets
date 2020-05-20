
d <- readRDS("data/countries.rds")
g1 <- d[,c("CONTINENT", "UNREGION1", "NAME", "NAME", "ISO3" )]
g2 <- d[,c("CONTINENT", "UNREGION1", "NAME", "NAME_ISO", "ISO3" )]
g3 <- d[,c("CONTINENT", "UNREGION1", "NAME", "NAME_FAO", "ISO3" )]
g4 <- d[,c("CONTINENT", "UNREGION1", "NAME", "NAME_LOCAL", "ISO3" )]
colnames(g1)[4] <- "matchname"
colnames(g2) <- colnames(g1)
colnames(g3) <- colnames(g1)
colnames(g4) <- colnames(g1)
g <- rbind(g1, g2, g3, g4)
g <- unique(g)
g <- g[g$matchname != "", ]

g$NAME[g$NAME == "Czech Republic"] <- "Czechia"
g$NAME[g$NAME == "Macedonia"] <- "North Macedonia"
g$NAME[g$NAME == "Swaziland"] <- "Eswantini"
#g$lower <- g$matchname
#g$lower[g$ISO3 == "ALA"] <- tolower("Åland")
#g$lower[g$ISO3 == "CIV"] <- "côte d'ivoire"
#g$lower[g$ISO3 == "CUW"] <- "curaçao"
#g$lower[g$ISO3 == "BLM"] <- "saint barthélemy"
#g$lower[g$ISO3 == "REU"] <- "réunion"
#g$lower[g$ISO3 == "AUT"] <- "österreich"
#g$lower <- tolower(g$lower)

m <- matrix(c("Réunion", "Curaçao", "Eswatini", "Bolivia (Plurinational State of)", "China, mainland", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of", "China, Taiwan Province of China", "Congo", "Côte d'Ivoire", "Czechia", "Dem. People's Republic of Korea", "Democratic People's Republic of Korea", "Ethiopia PDR", "Iran (Islamic Republic of)", "Lao People's Democratic Republic", "Netherlands Antilles (former)", "Republic of Korea", "Republic of Moldova", "Sudan (former)", "The former Yugoslav Republic of Macedonia","North Macedonia", "United Republic of Tanzania", "Venezuela (Bolivarian Republic of)", "REU", "CUW", "SWZ", "BOL", "CHN", "HKG", "MAC", "TWN", "TWN", "COG", "CIV", "CZE", "PRK", "PRK", "ETH", "IRN", "LAO", "ANT", "KOR", "MDA", "SDN", "MKD", "MKD", "TZA", "VEN"), ncol=2)
m <- data.frame(m)
colnames(m) <- c("matchname", "ISO3")
m$NAME <- c("Réunion", "Curaçao", "Eswatini", "Bolivia", "China", "China", "Hong Kong", "Macao", "Taiwain", "Congo, Republic of", "Côte d'Ivoire", "Czechia", "North Korea", "North Korea", "Ethiopia", "Iran", "Laos", "Netherlands Antilles", "South Korea", "Moldova", "Sudan", "North Macedonia", "North Macedonia", "Tanzania", "Venezuela")
m$CONTINENT <- c("Africa", "South America", "Africa", "South America", "Asia", "Asia", "Asia", "Asia", "Asia", "Africa", "Africa", "Europe", "Asia", "Asia", "Africa", "Asia", "Asia", "South America", "Asia", "Europe", "Africa", "Europe", "Europe", "Africa", "South America") 

m$UNREGION1 = ""
m <- m[, colnames(g)]

mm <- m[0,]
mm <- mm[1:11, ]
mm$matchname <- c('Bolivia; Plurinational State of', "Cote d'Ivoire", 'Iran; Islamic Republic of', 'Korea; Democratic People', 'Korea; Republic of', 'Lao People', 'Macedonia; former Yugoslav Republic', 'Netherlands Antilles', 'Occupied Palestinian Territory', 'Tanzania; United Republic of', 'Venezuela; Bolivarian Republic of')
mm$NAME <- c('Bolivia', "Côte d'Ivoire", 'Iran', 'North Korea', 'South Korea', 'Laos', 'North Macedonia', 'Netherlands Antilles', 'Palestine', 'Tanzania', 'Venezuela')
mm$ISO3 <- c('BOL', 'CIV', 'IRN', 'PRK', 'KOR', 'LAO', 'MKD', 'ANT', 'PSE', 'TZA', 'VEN')
mm$CONTINENT <- c('South America', 'Africa', 'Asia', 'Asia', 'Asia', 'Asia', 'Europe', 'South America', 'Asia', 'Africa', 'South America')

m <- rbind(m, mm)

g <- rbind(g, m)
g <- g[order(g$NAME), ]
rownames(g) = NULL

r <- unique(g[, c("ISO3", "UNREGION1")])
r <- r[r[,2] != "", ]
i <- match(g$ISO3, r$ISO3)
j <- na.omit(cbind(1:length(i), i))
g$UNREGION1[j[,1]] <- r$UNREGION1[j[,2]]


saveRDS(g, "data/countries2.rds")



