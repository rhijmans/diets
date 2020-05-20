\name{nutrientContent}

\alias{nutrientContent}

\title{
(Micro-) nutrients in food or food groups
}

\description{
This function returns a \code{data.frame} with (micro-)nutrient content by food group (FdGp1). It was computed with a Food Composition Table (FCT) by averaging all the nutrients by the first level of food group.

\code{code} = food group code

\code{group} = food group name

\code{tag} = INFOOD Food Component Identifier

\code{value} = amount of the (micro-) nutrient

\code{unit} = unit of \code{value}

\code{desc} = description of the (micro-) nutrient
}

\usage{
nutrientContent(continent="", redpalmoil=0.5, orangesweetpot=0, fbs=FALSE)
}

\arguments{
  \item{continet}{character. One of "Africa", "Americas", "Asia", "Europe", "Oceania", or "". If not "", continental level adjustments are made to the fruits and vegetables composition of food groups}
  \item{redpalmoil}{numeric. Value between 0 and 1 to indicate the ratio of red palm oil}
  \item{orangesweetpot}{numeric. Value between 0 and 1 to indicate the ratio of orange sweet potatoes}
  \item{fbs}{logical. if \code{TRUE}, only FAO Food Balance Sheet food groups are returned}
}

\value{
data.frame
}

\references{
US Department of Agriculture, Agricultural Research Service, Nutrient Data Laboratory (2015) USDA National Nutrient Database for Standard Reference, Release 27 (slightly revised). Version Current:  May 2015.

Food and Agriculture Organisation (FAO) (2012) West African Food Composition Table. Rome.

University of California at Berkeley International Minilist. WorldFood Dietary Assessment System, 2nd edition.

Wessells KR, Singh GM, Brown KH (2012) Estimating the Global Prevalence of Inadequate Zinc Intake from National Food Balance Sheets: Effects of Methodological Assumptions.
}

\examples{
content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0.2)


# this is the table that is used to do the continent based adjustments
FCTcnt <- readRDS(system.file("ex/ContiConsump.rds", package="diets"))
}

