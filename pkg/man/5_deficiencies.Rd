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
  \item{intake}{data.frame}
  \item{requirements}{data.frame}
  \item{CV}{data.frame. Must contain the following column: micronutrient tagname, CV value, units}
}

\details{
The code will use "CV[,3]" -Units- to reshape the deficiencies values.
}

\value{
data.frame
}

\references{
Arsenault JE, Hijmans RJ, Brown KH, 2015. Improving nutrition security through agriculture: an analytical framework based on national food balance sheets to estimate nutritional adequacy of food supplies. Food Security 7:693-707.
}

\examples{
consumption <- readRDS(system.file("ex/rwa_FBS.rds", package = "diets"))
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("Item", "Value")]
colnames(consumption) <- c("group", "value")

content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0.2)

intake  <- nutrientIntake(consumption, content, verbose=FALSE)

intake <- add_Ca(intake)
intake <- adjust_Ca(intake)
intake <- adjust_Zn(intake)
iron <- readRDS(system.file("ex/NHemeIron.rds", package="diets"))
intake <- adjust_Fe(intake, iron)

reqear <- readRDS(system.file("ex/reqs_ear.rds", package = "diets"))
pop <- readRDS(system.file("ex/rwa_pop.rds", package = "diets"))
popreq <- nutrient_requirements(pop, reqear)

CV <- readRDS(system.file("ex/CV.rds", package = "diets"))


d <- deficiencies(intake, popreq, CV)
head(d)

}

