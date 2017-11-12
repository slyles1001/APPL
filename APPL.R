Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')
require("rSymPy")
as.APPL <- function(x){
  if(!inherits(x, "APPL")) class(x) <- c("APPL", class(x))
  x
}

AllCombinations <- function(n, k, pr = FALSE){
  n <- as.integer(n)
  k <- as.integer(k)
  if(k > n){print(paste("ERROR(AllCombinations):",
                  "k must be less than or equal to n"))
    return()}
  h <- 1
  B <- matrix(nrow = choose(n,k), ncol = k)
  A <- 1:k
  #A[, 1] <- rep(1:(n-1), (n-1):1)
  if(pr) print(A)
  B[1,] <- A
  j <- 2
  while(A[1] != n - k + 1){
    if(A[k] != n) A[k] <- A[k] + 1
    else{
      MoveLeft <- TRUE
      h <- 1
      while (h <= k-1 && MoveLeft){
          if(A[k - h] < n - h){
            A[k - h] <- A[k - h] + 1
            for(m in (h - 1):0){
              A[k - m] <- A[k - (m + 1)] + 1
            }
          MoveLeft <- FALSE
        }
        h <- h + 1
      }
    }
    if(pr) print(A)
    B[j,] <- A
    j <- j + 1
  }
  B
}


AllPermutations <- function(n, pr=FALSE){
  options(warn = -1)
  n <- as.integer(n)
  if(is.na(n)){print("Please input an integer."); return()}
  B <- matrix(nrow = factorial(n), ncol = n)
  A <- 1:n
  if(pr) print(A)
  B[1, ] <- A
  bp = 2
  Continue <- TRUE
  for(All in 1:(factorial(n)-1)){
    swapped <- FALSE
    #j = n-1
    for (j in (n-1):1){

      if ((A[j] < A[j + 1]) && !(swapped)){
        OrigVal <- A[j]
        MiniMaxIndex <- j + 1
        # print(`MiniMaxIndex is`, MiniMaxIndex):
        for (k in n:(j + 1)){
      # print(`swapping is needed since A[j] < A[j + 1]`):
          swapped <- TRUE
          if ((A[k] < A[MiniMaxIndex]) && (A[k] > OrigVal)) MiniMaxIndex <- k
        }
      
        Temp1 <- A[MiniMaxIndex]
        A[MiniMaxIndex] <- A[j]
        A[j] <- Temp1
     
        if (swapped){
          Temp2 <- A
          for(m in (j + 1):n)  A[m] <- Temp2[n + j + 1 - m]
          }# if swapped
      }# if A[j]
      #j = j - 1
    } #for j
    if(pr) print(A)
    B[bp, ] = A
    bp = bp + 1
  } #for All
  options(warn = 0)
  B
}

ArcTanVariate <- function(alpha, phi){
  # Leemis and Andrew's
  # Make it work! we got sympy now
  variate <- phi + 1/alpha * tan(pi / 2 - (runif(10^12)/1000000000000) *
    (arctan(alpha * phi) + pi / 2))
}

Benford <- function(x){
  #  Purpose: Given a continuous random variable X, this procedure returns
  #           the PDF of Z, as defined in Leemis, Schmeiser, and Evans
  #           (2000)
  #
  #  Arguments: X: A continuous random variable.
  # 
  #  Algorithm:
  #    1.  Check for 1 argument, the RV X
  #    2.  Check that the RV X is in the list of 3 lists format
  #    3.  Check that the RV X is continuous 
  #    4.  Convert the RV X to PDF form
  #    5.  Check that the RV supports are numeric and finite
  #    6.  Compute the lower and upper summation limits
  #    7.  Create lists Weight, TruncX, and Xprime that will hold 
  #        (Hi - Lo + 1) elements
  #    8.  Compute each "weight" and "truncated distribution" for the
  #        intervals [Lo, Lo + 1], [Lo + 1, Lo + 2], ..., [d, d + 1], ...,
  #        [Hi - 2, Hi - 1], [Hi - 1, Hi] 
  #    9.  For each value d, where d is a value s.t. Lo <= d <= Hi,
  #        transform the truncated distribution by the function x -> x - d
  #    10. Send the list of weights along with its list of truncated
  #        distributions to the procedure Mixture to compute the PDF of Z,
  #        as defined in Leemis, Schmeiser, and Evans (2000)
  #
  
}


PDF <- function(X, x = NA){
  #  Purpose: PDF is a procedure that:
  #          (1) Returns the probability density function (continuous)
  #              or the probability mass function (discrete) of a 
  #              random variable X in the APPL list of 3 lists format if
  #              the only argument given is X, or
  #          (2) Returns the value Pr(X = x) if it is given the optional
  #              argument x in addition to the RV X
  #
  #  Arguments: X: A continuous or discrete random variable; 
  #             x (optional argument): A numeric value entered when
  #                trying to determine Pr(X = x)
  #
  #  Algorithm: 1. Perform error checking on user entered arguments
  #             2. The PDF of X is determined by sending it to its 
  #                appropriate category, which is based on whether:
  #                A. X is continuous or discrete
  #                B. X is entered in its PDF, CDF, SF, HF, CHF, or IDF
  #                   format 
  #             3. If only 1 argument (the random variable X) is entered
  #                by the user, return the probability density function of
  #                X in the APPL list of 3 lists format 
  #             4. If 2 arguments are provided, return the value Pr(X = x)
  
  if(!(class(X) == "RV")){print("Input must be a random variable")}
  
  
  
}


# R can do integrals. So, if we write a fxn that integrates, 
# have AsAlg=TRUE in parameters
# T: give sympy; F: give R















