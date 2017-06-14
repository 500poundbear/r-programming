add_num <- function(a, b) {
  a + b
}

above10 <- function(vec) {
  vec[vec > 10]
}

firstclassfn <- function(x) {
  y <- function(y) {
    y + x
  }
  y
}

meanMat <- function(mat) {
  res<-vector(mode="numeric", length=0)
  for(q in 1:ncol(mat)) {
    me <- mean(mat[,q])
    res <- c(res, me)    
  }
  res
}