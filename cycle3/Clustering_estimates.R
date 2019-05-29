#Created by Pablo Diego Rosell, PhD, for Gallup inc. in May 2019
# Setup environment

if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(multiwayvcov, lmtest, RCurl)

# download scripts and data

dlScripts <- function (scriptNames) {
  fileHolder <- getURL(paste(githubRepo, scriptNames, sep = "/"), ssl.verifypeer = FALSE)
  fileConn<-file(scriptNames)
  writeLines(fileHolder, fileConn)
  close(fileConn)
}
githubRepo <- "https://raw.githubusercontent.com/GallupGovt/ngs2/master/cycle2/analysis"
scriptNames <- c("gamesData.csv")
lapply(scriptNames, dlScripts)
factorial <- read.csv(file = paste(getwd(), "gamesData.csv", sep = '/'))

#Clustering estimates

main.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
glm.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools

# Standard error for h1.1 without clustering

logitR1 <- glm(glm.formula, data = factorial, family = "binomial")
summary(logitR1)

# Standard error for h1.1 with clustering

logitR1.multiwayvcov <- cluster.vcov(logitR1, cbind(factorial$matchid))
coeftest(logitR1, logitR1.multiwayvcov)


