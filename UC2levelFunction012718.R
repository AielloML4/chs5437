# function UC2
# Accepts a data.frame, with colum of items named "Obj",
# Each item should have the same designation
# The number of repeated measurements (2 way data) for each item should be equal
# Returns a list with U and C
UC2 <- function(dat, variables){
  ##Calculate U
  dat.obj <- split(dat,dat$obj)
  #variables <- c(3,4)
  # create matrix pxp variables
  sw <- matrix(0, nrow=length(variables), ncol=length(variables))
  for(i in 1:length(dat.obj)){
    mn <- apply(dat.obj[[i]][,variables],2,mean)
    for(j in 1:dim(dat.obj[[i]])[1]){
      sw<- sw+ as.matrix(t(dat.obj[[i]][j,variables]-mn))%*%as.matrix(dat.obj[[i]][j,variables]-mn)
    }
  }
  u <- sw/(length(dat.obj)*(dim(dat.obj[[1]])[1]-1))
  u
  
  # Calc C
  # create sstar
  sstar <- matrix(0, nrow=length(variables), ncol=length(variables))
  xbar <- apply(dat[,variables],2,mean)
  for(i in 1:length(dat.obj)){
    mn <- apply(dat.obj[[i]][,variables],2,mean)
    sstar <- sstar+as.matrix(mn-xbar)%*%t(as.matrix(mn-xbar))
  }
  #sstar
  c <- sstar/(length(dat.obj)-1)-sw/(length(dat.obj)*dim(dat.obj[[1]])[1]*(dim(dat.obj[[1]])[1]-1))
  c
  
  return(list(U=u, C=c))
}