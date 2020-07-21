
library(gtdq)
library(diets)
## Return example data frames.
path <- file.path(system.file(package = "gtdq"), "external")

Consumption <- base::readRDS(file.path(path, "example/Consumption.RDS"))
Content <- base::readRDS(file.path(path, "example/Content.RDS"))
Pop <- base::readRDS(file.path(path, "example/Pop.RDS"))
ReqAge <- base::readRDS(file.path(path, "example/ReqAge.RDS"))
CV <- base::readRDS(file.path(path, "example/CV.RDS"))

Intake  <- Intake(Consumption, Content, "ENERGY", Wgpath=NULL, zn=FALSE)
Intake <- AdjustIntake("RWA", Intake, ca=F, fe=F, fortif=F)

newcons <- Consumption
colnames(newcons) <- c("group", "value")
newcont <- Content
colnames(newcont) <- c("group", "tag", "value")
new_intake  <- nutrientIntake(newcons, newcont, verbose=F)
m = merge(Intake[,c(3,1,2)], new_intake[,c(1,2,5)], by=1:2)
plot(m[,3:4])

Requirements <- Requirements(Pop, ReqAge, zn=FALSE)

ReqIntk  <- ReqIntk(Intake, Requirements)


req <- ReqIntk[ReqIntk$Tagname == "FE", ]

Deficiencies <- Deficiencies(ReqIntk, CV)


r <- Requirements
r$req <- r$Req_MNutr_Val * r$Pop
a <- aggregate(r[,c("Pop", "req")], r[,"Tagname",drop=FALSE], sum)
a$req <- a$req / a$Pop
colnames(a)[1] <- "tag"
i <- Intake
colnames(i)[1:2] <- c("tag", "intake")
cv <- CV
colnames(cv)[1] <- "tag"

d <- deficiencies(i, a, cv, old=TRUE)
d$frac <- d$intake / d$req
Deficiencies$frac <- Deficiencies$AvgMNutr_Intk / Deficiencies$AvgMNutr_Req
head(d)
head(Deficiencies)


defs <- function(intake, req, cv) {
	100 * (1 - pnorm(intake, req, cv * req))
}


