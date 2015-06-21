# This makes a vector with matrices 
# The matrix is 2 x 2 
#in the following I will create entries for my matrix
a <- makeCacheMatrix( matrix(c(4,8,17,27), nrow = 2, ncol = 2) ) {
  # Either have cached value or "NULL"if nothing is cached
  # now there is nothing that is cached.
  # the operate we choose is NA
  cache <- NA
  # Denoting the function with a new value in our matrix
  setMatrix <- function(newValue) {
    z <<- newValue
    # The matrix is placed into a new value, 
    # we take out the cache
    cache <<- NA
  }
  # getMatrix is a function that will return our matrix
  getMatrix <- function() {
    z
  }
  # cacheInverse helps us solve compute our matrix
  # cache is now impled with a solve operator
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  # getInverse will help us gather a cache
  getInverse <- function() {
    cache
  }
  # These under the "list" function will return
  # our 4 needed categories
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
# The following function calculates the inverse of a "special" matrix created with 
# CacheMatrix
cacheSolve <- function(p, ...) {
  # get the cached value
  inverse <- p$getInverse()
  # if a cached value exists return it
  if(!is.na(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # the alternative is to retrieve the matrix,
  # then caclulate the inverse and store it in
  # we again look at the cache
  data <- p$getMatrix()
  inverse <- solve(data)
  p$cacheInverse(inverse)
  # return the inverse
  inverse
}
# this displays a summary of the matrix's attriutes 
summary(a)
#the $ symbol arrange my set of vectors into a matrix
a$getMatrix()
# cacheSolve function will help calcuate the inverse.
cacheSolve(a)
