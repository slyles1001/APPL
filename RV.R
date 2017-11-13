# Need jdk installed for jython
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')
require("rSymPy")


#  Purpose:  Define the common parametric continuous univariate
#            probability distributions shown below:
#
#  Distribution      Support        Parameters     ParameterRestrictions    
#  ------------      -------        ----------     ---------------------
#  ArcSin            0 < x < 1      none
#  ArcTan            x >= 0         alpha, phi     alpha > 0; 
#                                                  -inf < phi < inf
#  Beta              0 <= x <= 1    Shape alpha,   alpha > 0; beta > 0
#                                   shape beta
#  Cauchy            -inf < x < inf Location a,    -inf < a < inf; 
#                                   Scale alpha    alpha > 0 
#  Chi               x >= 0         Shape n        positive integer n
#  ChiSquare         x >= 0         Shape n        positive integer n
#  DblyNoncentralF
#  DblyNoncentralT
#  Erlang            x >= 0         Scale lambda,  lambda > 0
#                                   shape n        positive integer n
#  Error             -inf < x < inf mu, alpha,     mu > 0; alpha >= 1;
#                                   location d     -inf < d < inf
#  Exponential       x >= 0         Scale lambda   lambda > 0
#  Exponent Pwr      x >= 0         Scale lambda,  lambda > 0; kappa > 0
#                                   shape kappa
#  ExtremeValue      -inf < x < inf Scale ? alpha, alpha > 0; beta > 0
#                                   shape ? beta
#  F                 x >= 0         Shape n1,      positive ints n1, n2
#                                   shape n2
#  Gamma             x >= 0         Scale lambda,  lambda > 0; kappa > 0
#                                   shape kappa
#  GenPareto         x >= 0         gamma, delta,  kappa
#  Gompertz          x >= 0         Shape delta,   delta > 0; kappa > 1
#                                   shape kappa
#  HyperbolicSecant  -inf < x < inf none
#  HyperExponential  x >= 0         list p, list l all elts of list p: 
#                                                  0 <= p <= 1
#                                                  all elts of list l:
#                                                  l > 0
#  HypoExponential   x >= 0         list l         all elts of list l:
#                                                  l > 0
#  IDB               x >= 0         Shape gamma,   gamma, delta >= 0, 
#                                   delta, kappa   kappa >= 0
#  InverseGaussian   x > 0          Scale lambda,  lambda > 0; mu > 0
#                                   location mu
#  InvertedGamma     x >= 0         alpha, beta    alpha > 0; beta > 0
#  KS
#  LaPlace           -inf < x < inf omega, theta   omega > 0
#  LogGamma          -inf < x < inf alpha, beta    alpha > 0; beta > 0
#  Logistic          -inf < x < inf Scale kappa    kappa > 0; lambda > 0
#                                   shape (?) lambda
#  Log logistic      x >= 0         Scale lambda,  lambda > 0; kappa > 0
#                                   shape kappa
#  LogNormal         x >= 0         Scale mu,      -inf < mu < inf; 
#                                   shape sigma    sigma > 0
#  Lomax             x > 0          kappa, lambda  kappa > 0; lambda > 0
#  Makeham           x >= 0         Shape gamma,   gamma, delta > 0;
#                                   delta, kappa   kappa > 1
#  Muth              x >= 0         Shape kappa    0 < kappa <= 1
#  NoncentralChiSqre 0 <= x <= 1
#  NoncentralF       0 <= x <= 1
#  NoncentralT       0 <= x <= 1
#  Normal            -inf < x < inf Location mu,   -inf < mu < inf;
#                                   scale sigma    sigma > 0
#  Pareto            x >= lambda    Location lambda,  
#                                   shape kappa    lambda > 0; kappa > 0
#  Rayleigh          x > 0          Scale lambda   lambda > 0
#  StandardCauchy    -inf < x < inf none
#  StandardNormal    -inf < x < inf none
#  StandardTriangular0 <= x <= 1    m              0 <= m <= 1
#  StandardUniform   0 <= x <= 1    none
#  T                 -inf < x < inf Shape n        positive integer n
#  Triangular        a <= x <= b    Min a, mode m, a < m < b
#                                   max b
#  Uniform           a <= x <= b    Min a, max b   -inf < a < b < inf
#  Weibull           x >= 0         Scale lambda,  lambda > 0; kappa > 0
#                                   shape kappa
#
#  Arguments:  The parameters of the distribution of the random variable
#
#  Algorithm:
#    1.  Check for the appropriate number of arguments
#    2.  Check parameter space when parameters are numeric
#    3.  Check to see that the parameters are finite
#    4.  Make assumptions about any symbolic parameters
#    5.  Assign a list of lists in the following format:
#        [[f(x)], [support], ["Continuous", "XXX"]]
#        where XXX is one of the following:  PDF, CDF, IDF, SF, HF, CHF
#    6.  Return the list of lists
#####################
# Global Init some constants to standardize
#####################
infinity <- Var("oo")
pi <- Var("Pi")
# Python overloaded comparisons... AND DIDN'T TELL ME?
is.infinity <- function(x){
  y <- x[1] == infinity[1]
}
unvar <- function(x){ # Again, to unload overload..
  strtoi(x)
}
#####################
# S4 class -- I think I prefer S3.
# setClass("RV",
#          slots = list(pdf = "character",
#                       support = "character",
#                       type = "character"))
# S4 arcsin definition:
# LoL <- new("RV", pdf =paste("x -> ",sympy("1/(pi*sqrt(x*(x-1)))")),
#            support=c("0","1"), 
#            type=c("Continuous", "PDF"))
#####################
ArcSinRV <- function(){
  #  ArcSin distribution (special case of beta with both parameters 1 / 2)
  if (nargs() > 0){print("ArcSin requires no arguments"); return()}
  # We must tell sympy that x is a symbol
  x <- Var("x")
  # Make List of Lists, same as APPL -- S3
  LoL <- structure(list(pdf=paste("x -> ",sympy("1/(pi*sqrt(x*(x-1)))")),
                  support=c(0,1),
                  type=c("Continuous", "PDF")), class="RV")

  return(LoL)
  }
  
ArcTanRV <- function(alpha, phi){
  # Will take numeric or Var type inputs
  if (nargs() != 2){
    print('ERROR(ArcTanRV): This procedure requires 2 arguments')
    return()
  }
  if (is.infinity(alpha) || is.infinity(phi)){
    print('ERROR(ArcTanRV): Both parameters must be finite')
    return()
  }
  if((class(alpha)!="Sym" && alpha <= 0) || (class(phi)!="Sym" && phi <= 0)){
    print("Alpha and Beta must be symbols or positive numerics")
    return()
  }
  
  a <- ifelse(is.numeric(alpha), # If alpha is a number
              Var(toString(alpha)), # turn it into a string for sympy
              Var("alpha"))           # otherwise, leave it symbolic
  p <- ifelse(is.numeric(phi), # If phi is a number
              Var(toString(phi)), # turn it into a string for sympy
              Var("phi"))           # otherwise, leave it symbolic

  x = Var("x")
  # (alpha1 / ((arctan(alpha1 * phi1) + Pi / 2) *
  # (1 + alpha1 ^ 2 * (x - phi1) ^ 2))
  # Hmm... does R have C-type std? This string stuff is ugly...
  eq = paste(a, " / ((atan(", a, " * ", p, ") + pi / 2) * (1 + ",
             a, "**2 * (", x, " - ", p, ")**2))")
  LoL <- structure(list(pdf=paste("x -> ",sympy(eq)), 
                        support=c(0, infinity), 
                        type=c("Continuous", "PDF")), class="RV")
  return(LoL)
}  
  
BetaRV <- function(alpha, beta){
  if (nargs() != 2){
    print('ERROR(BetaRV): This procedure requires 2 arguments')
    return()
  }
  if (is.infinity(alpha) || is.infinity(beta)){
    print('ERROR(BetaRV): Both parameters must be finite')
    return()
  }
  if((class(alpha)!="Sym" && alpha <= 0) || (class(beta)!="Sym" && beta <= 0)){
    print("Alpha and Beta must be symbols or positive numerics")
    return()
  }
  a <- ifelse(is.numeric(alpha), # If alpha is a number
              Var(toString(alpha)), # turn it into a string for sympy
              Var("alpha"))           # otherwise, leave it symbolic
  b <- ifelse(is.numeric(beta), # If phi is a number
              Var(toString(beta)), # turn it into a string for sympy
              Var("beta"))           # otherwise, leave it symbolic
  x = Var("x")
  G = gamma(alpha + beta) / (gamma(alpha)* gamma(beta))
  
  
  eq = paste(G, " * x**(", a, " - 1)*(1 - x)**(", b, " - 1)")
  LoL <- structure(list(pdf=paste("x -> ", sympy(eq)),
                        support=c(0, 1), 
                        type=c("Continuous", "PDF")), class="RV")
  return(LoL)
}
  
CauchyRV <- function(a, alpha){
  if (nargs() != 2){
    print('ERROR(CauchyRV): This procedure requires 2 arguments')
    return()
  }
  if (is.infinity(alpha) || is.infinity(a)){
    print('ERROR(CauchyRV): Both parameters must be finite')
    return()
  }
  if((class(a)!="Sym" && !is.numeric(a)) || (class(alpha)!="Sym" && alpha <= 0)){
    print("A and alpha must be symbols or positive numerics")
    return()
  }
  a1 <- ifelse(is.numeric(a), # If alpha is a number
              Var(toString(a)), # turn it into a string for sympy
              Var("a"))           # otherwise, leave it symbolic
  a2 <- ifelse(is.numeric(alpha), # If phi is a number
              Var(toString(alpha)), # turn it into a string for sympy
              Var("alpha"))           # otherwise, leave it symbolic
  
  x <- Var("x")
  eq <- paste("1 / (", a2, " * Pi * (1 + ((x - ", a1, ") / ", a2, ")**2))")
  LoL <- structure(list(pdf=paste("x -> ", sympy(eq)), 
                        support=c(-infinity, infinity),
                        type=c("Continuous", "PDF")), class="RV")
  return(LoL)
}  
  

ChiRV <- function(n){
  if (nargs() != 1){
    print('ERROR(ChiRV): This procedure requires 1 argument')
    return()
  }
  if (is.infinity(n)){
    print('ERROR(ChiRV): n must be finite')
    return()
  }
  if(!is(n, "Sym") && (!is.numeric(n) || n < 0)){
    print("The shape parameter n must be a symbol or positive numeric")
    return()
  }
  # n1 <- ifelse(is.numeric(n), # If n is a number
  #              Var(n), # turn it into a string for sympy
  #              Var("n"))           # otherwise, leave it symbolic
  # 
  if(is.numeric(n)) n1 <- Var(n)
  else if(is(n, "Sym")) n1 <- n
  
  x <- Var("x")
  G <- gamma(unvar(n1)/2)
  ###############
  # OK, some huge 'feature' of sympy...
  # > sympy('1/(2**(5/2 - 1))')
  # [1] "0"
  # > sympy('1/(2**(3/2))')
  # [1] "0"
  # > sympy('1.0/(2**(3/2))')
  # [1] "0.5"
  # > sympy('1.0/(2.0**(3/2))')
  # [1] "0.5"
  ###############
  twos = 2**(unvar(n1) / 2 - 1 ) # python intstuff
                                 # don't know how to algebra that.
  eq <- paste("x**", (n1 - 1), 
              " * exp(-x**2 / 2) / (", twos, " * ", G, ")")
  print(eq)
  LoL <- structure(list(pdf=paste("x -> ", sympy(eq)), 
                            support=c("0", infinity),
                            type=c("Continuous", "PDF")), class="RV")
  return(LoL)
}  








