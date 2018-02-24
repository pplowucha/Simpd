library(MCDA)

epsilon <-0.01

t = read.table("lodowki.csv", header = TRUE, sep = ",", row.names = 1)


# ranks of the alternatives
alternativesRanks <- c(1,2,3,4,5,6,7,8,9,10,11,13,13,14,15)
names(alternativesRanks) <- row.names(t)

# criteria to minimize or maximize
criteriaMinMax <- c("min","max","min")
names(criteriaMinMax) <- colnames(t)

x<-additiveValueFunctionElicitation(
  t,criteriaMinMax, epsilon, alternativesRanks = alternativesRanks)