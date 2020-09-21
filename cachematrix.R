# Objective : To write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function()inver
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## A matrix that is inverse of x is returned

cacheSolve <- function(x, ...){
  inver <- x$getinverse()
  if(!is.null(inver)){ # To check whether inverse is NULL 
    message("getting cached data")
    return(inver) # Returns inverse
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver # Returns a matrix that is inverse of x
}
