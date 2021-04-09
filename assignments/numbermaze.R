library(dequer)
Mbook <- matrix(c(3,5,7,4,6,
                  5,3,1,5,3,
                  2,8,3,1,4,
                  4,5,7,2,3,
                  3,1,3,2,0), byrow = TRUE, nrow = 5)
set.seed(410)
Mrand <- matrix(sample(1:9,100,replace=TRUE),nrow=10)


minMoves <- function(s1,s2,M) {
  n <- nrow(M)
  marked <- matrix(rep(FALSE, n^2), nrow = n)
  q <- queue()
  pushback(q, c(s1,s2,0)) # third number is depth
  while(length(q) != 0) {
    v <- pop(q)
    i <- v[1]
    j <- v[2]
    depth <- v[3]
    if(!marked[i,j]) {
      marked[i,j] <- TRUE
      if(i==n && j==n)
        return(depth)
      x <- M[i,j]
      if(i+x <= n) pushback(q, c(i+x,j,depth+1))
      if(i-x >= 1) pushback(q, c(i-x,j,depth+1))
      if(j+x <= n) pushback(q, c(i,j+x,depth+1))
      if(j-x >= 1) pushback(q, c(i,j-x,depth+1))
    }
  }
  return("NO SOLUTION")
}


