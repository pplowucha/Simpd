library("MCDA")
library("OutrankingTools")
library("topsis")
library("MCDM")

topsis <- function(decision, #matrix with all the alternatives
                  weights,  #vector with the numeric values of the weights
                  cb,       #vector with the "type" of the criteria (benefit = "max", cost = "min")
                  v         #value with the real number of the 'v' parameter to calculate Q
)
{
  #Checking parameters
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  if(sum(weights) != 1)
    stop("The sum of 'weights' is not equal to 1")
  if(! is.character(cb))
    stop("'cb' must be a character vector with the type of the criteria")
  if(! all(cb == "max" | cb == "min"))
    stop("'cb' should contain only 'max' or 'min'")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(length(cb) != ncol(decision))
    stop("length of 'cb' does not match the number of the criteria")
  if(missing(v))
    stop("a value for 'v' in [0,1] should be provided")
  
  #1. Ideal solutions
  posI <- as.integer(cb == "max") * apply(decision, 2, max) +
    as.integer(cb == "min") * apply(decision, 2, min)
  negI <- as.integer(cb == "min") * apply(decision, 2, max) +
    as.integer(cb == "max") * apply(decision, 2, min)
  
  #2. S and R index
  norm =function(x,w,p,n){
    w*((p-x)/(p-n))
  }
  SAux <- apply(decision, 1, norm, weights, posI, negI)
  S <- apply(SAux, 2, sum)
  R <- apply(SAux, 2, max)
  
  
  #3. Q index
  #If v=0
  if (v==0)
    Q <- (R-min(R))/(max(R)-min(R))
  #If v=1
  else if (v==1)
    Q <- (S-min(S))/(max(S)-min(S))
  #Another case
  else
    Q <- v*(S-min(S))/(max(S)-min(S))+(1-v)*(R-min(R))/(max(R)-min(R))
  
  #4. Checking if Q is valid
  if( (Q == "NaN") || (Q == "Inf")){
    RankingQ <- rep("-",nrow(decision))
  }else{
    RankingQ <- rank(Q, ties.method= "first") 
  }
  #5. Ranking the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), S = S, R = R, Q = Q, Ranking = RankingQ))
  
}
d <- d <- matrix(c(3000, 1408, 2490, 1029, 1641, 1349, 1624, 1539, 1899, 2071, 1244, 1299, 2260, 1505, 1819, 180, 190, 149, 170, 171, 149, 122, 177, 200, 162, 190, 220, 189, 165, 219, 70, 94, 69, 84, 73, 89, 91, 99, 65, 80, 91, 79, 83, 72, 95, 100, 95),nrow = 15,ncol = 3)
w <- c(0.3, 0.3, 0.4)
i <- c("-", "+", "-")
topsis(d, w, i)
