\name{deficiencies}

\alias{deficiencies}

\title{
Compute micronutrient deficiencies
}

\description{
This function returns micronutrient deficiencies based on requirements and intake
For CV, see references.
}

\usage{
deficiencies(intake, requirements, CV)
}

\arguments{
  \item{ReqIntk}{data.frame. Columns must be ordered. Order : micronutrient tagname, micronutrient requirement value, population, micronutrient per person by day}
  \item{CV}{data.frame. Must contain the following column: micronutrient tagname, CV value, units}
}

\details{
The code will use "CV[,3]" -Units- to reshape the deficiencies values.
}

\value{
data.frame
}

\references{
Arsenault JE, Hijmans RJ, Brown KH, 2015. Improving nutrition security through agriculture: an analytical framework based on national food balance sheets to estimate nutritional adequacy of food supplies. Food Security, 7:693-707.

Willett WC, Sampson L, Stampfer MJ, Rosner B, Bain C, Witschi J et al., 1985. Reproducibility and validity of a semiquantitative food frequency questionnaire. American journal of epidemiology, 122(1):51-65.

Nelson M, Black AE, Morris JA, Cole TJ, 1989. Between-and within-subject variation in nutrient intake from infancy to old age: estimating the number of days required to rank dietary intakes with desired precision. The American journal of clinical nutrition, 50(1), 155-167.

Calloway DH, Murphy SP, Beaton GH, Lein D, 1993. Estimated vitamin intakes of toddlers: predicted prevalence of inadequacy in village populations in Egypt, Kenya, and Mexico. The American journal of clinical nutrition, 58(3), 376-384.

Murphy SP, Beaton GH, Calloway DH, 1992. Estimated mineral intakes of toddlers: predicted prevalence of inadequacy in village populations in Egypt, Kenya, and Mexico. The American journal of clinical nutrition, 56(3), 565-572.
}


\examples{
#content  <-  1_nutrientContent(ISO3="RWA", conti=TRUE, palmoil=TRUE, sweetpot=TRUE)
#consumption <- base_2_getConsumption(ISO3="RWA", start=2005, end=2009, use="ENERGY")
#intake  <- base_3_Intake(consumption, content, use="ENERGY", zn=TRUE)
#adj_intake <- base_4_AdjustIntake(ISO3="RWA", intake, ca=TRUE, fe=TRUE, fortif=FALSE)

#req_age  <- base_5_getReqAge(rda=FALSE)
##pop <- base_6_getPopulation(ISO3='RWA', start=2005, end=2009)
#requirements <- base_7_Requirements(pop, req_age, zn=TRUE)

#ReqIntk  <- base_8_ReqIntk(adj_intake, requirements)

#CV <- readRDS(file.path(system.file(package = "diets"), "external/CV.rds"))
#CV$CV[is.na(CV$CV)] <- 0.3
#deficiencies <- base_9_Deficiencies(ReqIntk, CV)
}

