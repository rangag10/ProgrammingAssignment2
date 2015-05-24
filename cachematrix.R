makeCacheMatrix <- function(x = matrix()) 
#############################################################################
## This function creates a special object 'CacheMatrix' that does two things:
## 1. Takes a matrix as input, and initializes the value of the 'CacheMatrix'
##    object to this input, in a variable of global scope.
## 2. Creates four functions "get", "set", "setInverse" and "getInverse" 
##    associated with the CacheMatrix object that can be used to set or 
##    retrieve the value of the matrix or its inverse 
#############################################################################
{
  mInverse <- NULL                               
  set <- function(y)                             
  {
    x <<- y                                     
    mInverse <<- NULL
  }
  
  get <- function()
  {
    return(x)
  }
  
  setInverse <- function(inverse)
  {
    mInverse <<- inverse
  }
  
  getInverse <- function()
  {
    return(mInverse)
  }
  
  list (set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) 
#############################################################################
## This function takes as input an object of type 'CacheMatrix', and returns
## the inverse of the matrix represented by CacheMatrix.  In order to return
## the inverse, the function first checks if the inverse is already computed 
## and cached, and if so, it returns the inverse from the cache.  If the 
## inverse has not previously been cached, this function computes the inverse
## and caches it for later use
#############################################################################
{
  mInverse <- x$getInverse()
  if (!is.null(mInverse))
  {
    cat("Inverse found in cache; getting cached data \n")
    returnValue <- mInverse
  }
  else
  {
    cat("Inverse not found in cache; calculating and saving in cache \n")
    xMatrix <- x$get()
    xInverse <- solve(xMatrix)
    x$setInverse(xInverse)
    returnValue <- xInverse
  }
}

############################################################################
# Code to test the above functions                                         #
############################################################################
testMatrix <- matrix(c(4,3,3,2), nrow=2, byrow=T)
testObject <- makeCacheMatrix(testMatrix)
cat ("The test matrix is: \n")
matrixObject <- testObject$get()
print (matrixObject)
cat ("\n")

############################################################################
# For the first call to cacheSolve() below, the Inverse will be computed and 
# cached.
############################################################################
result1 <- cacheSolve(testObject)
cat ("The inverse matrix is: \n")
print(result1)
cat ("\n")

############################################################################
# For subsequent calls to cacheSolve() below, the Inverse will be returned  
# from the previously cached value.
############################################################################
result2 <- cacheSolve(testObject)
cat ("The inverse matrix is: \n")
print(result2)
cat ("\n")



