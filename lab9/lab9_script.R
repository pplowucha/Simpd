
library("C50")
data <- matrix(c(230, 129, 124, 157, 186, 202, 137, 162, 142, 210, 187, 201, 161, 203, 190, 182, 174, 200, 163, 172,
                 48, 41, 39, 44, 55, 39, 71, 37, 98, 115, 24, 75, 83, 41, 47, 49, 50, 43, 63, 79,
                 18, 17, 17, 14, 16, 14, 17, 17, 14, 20, 20, 18, 18, 20, 18, 15, 15, 20, 19, 15,
                 1690, 1790, 1349, 1299, 1299, 1160, 1499, 1259, 2699, 1419, 2069, 1489, 1189, 1369, 1619, 1399, 2000, 2090, 1590, 2419), 20, 4)
labels <- c("1", "2", "3", "4")
result <- c(2, 3, 4, 3, 3, 5, 3, 3, 1, 3, 3, 3, 3, 4, 2, 3, 1, 5, 3, 3)
test <- factor(result, labels)
colnames(data) <- c("Pojemnosc_chlodizarki", "pojemnosc_zamrazarki", "Utrzymywanie_temperatury_do_st_c", "Cena")
ruleModel <- C5.0(x = data[,], y = test, rules = TRUE)
ruleModel
summary(ruleModel)