# function UC1
# Accepts a data.frame containing single measurement data for multiple objects,
# Returns C
### NOTE - This gives the same result as the R function var()
### In this case the command would be var(dat[,variables])
#
UC1 <- function(dat, variables){
  sstar <- matrix(0, nrow=length(variables), ncol=length(variables))
  xbar <- apply(dat[,variables],2,mean)
  for(i in 1:dim(dat)[1]){
    sstar <- sstar+t(as.matrix(dat[i,variables]-xbar))%*%(as.matrix(dat[i,variables]-xbar))
  }
  #sstar
  c <- sstar/(dim(dat)[1]-1)
  c
  
  return(c)
}