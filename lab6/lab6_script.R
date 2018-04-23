library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(180, 90, 8,
                                        168, 100, 7,
                                        214, 82, 7,
                                        220, 84, 4,
                                        186, 85, 6,
                                        180, 90, 11,
                                        172, 88, 12,
                                        200, 96, 10,
                                        186, 100, 4,
                                        160, 110, 8,
                                        178, 90, 11,
                                        190, 87, 8,
                                        210, 83, 10,
                                        200, 41, 6,
                                        178, 76, 8,
                                        200, 92, 5,
                                        184, 76, 5,
                                        168, 93, 10,
                                        169, 96, 9,
                                        202, 96, 12), nrow=20, ncol=3))
trainingoutput <- c(1690, 1790, 1349, 1299, 1299, 1160, 1499, 1259, 2699, 1419, 2069, 1489, 1189, 1369, 1619, 1399, 2000, 2090, 1590, 2419)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Pojemnosc_chlodziarki", "pojemosc_zamrazarki", "mrozenie", "Cena") 
print(trainingdata)

#Train the neural network
#Going to have C(5, 4, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Cena+Pojemnosc_chlodziarki+pojemosc_zamrazarki+mrozenie, trainingdata, hidden=c(5, 4, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(1262, 1064, 1028,
                                   2060, 1075, 1600,
                                   1361, 2056, 2000), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)