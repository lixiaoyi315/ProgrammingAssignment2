+## Put comments here that give an overall description of what your functions do 
 +## Write a short comment describing this function 
 +##This function creates a list of functions that store and retrieve the matrix, and calculate and retrieve the inverse.
 +##This function is the same as the example given as the only changes required were the use of 
 +##solve instead of mean, and the limiting the argument to a Matrix.
 +makeCacheMatrix <- function(x = matrix()) { 
 +  inverse <- NULL 
 +  set <- function(y) { 
 +    x <<- y 
 +    m <<- NULL 
 +  } 
 +  get <- function() x 
 +  setinverse <- function(i) inverse <- i 
 +  getinverse <- function() inverse 
 +  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
 +} 
 +
 +
 +## Write a short comment describing this function 
 +##This function takes two arguments:
 +## The first is a matrix, and the second is the list that has the cache values in it.
 +##If the inverse has already been solved and the matrix matches the one in the cache the stored value is returned.
 +##If the matrix has changed the new value is calculated and the cache is updated to reflect the changes.
 +##If the value in the cache is null the values are calculated for first time and stored.
 +cacheSolve <- function(x, ...) { 
 +  ## Return a matrix that is the inverse of 'x' 
 +  inverse <- x$getinverse() 
 +  if(!is.null(inverse)) { 
 +    message("getting cached inverse") 
 +    return(inverse) 
 +  } 
 +  data <- x$get() 
 +  inverse <- solve(data, ...) 
 +  x$setinverse(inverse) 
 +  inverse 
 +} 
