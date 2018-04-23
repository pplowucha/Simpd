library("neuralnet")

#Going to create a neural network to perform the rooting of such a function: x^2+exp(-x)
#Type ?neuralnet for more information on the neuralnet library

#Generate 6 random numbers uniformly distributed between 0 and 100 (for very good results in such a range)
#And store them as a dataframe
traininginput <- as.data.frame(runif(10, min=1, max=10))
trainingoutput <- log10(traininginput)
#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have C(5, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.myfunc <- neuralnet(Output~Input, trainingdata, hidden=c(5, 3), threshold=0.01)
print(net.myfunc)

#Plot the neural network
plot(net.myfunc)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)*1) #Generate some numbers between 1 and 10
net.results <- compute(net.myfunc, testdata) #Run them through the neural network

#Lets see what properties net.myfunc has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,log10(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)