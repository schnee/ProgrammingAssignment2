## Put comments here that give an overall description of what your
## functions do

## creates a cacheable matrix object with get/set for the matrix and get/set for the
## inverse matrix, but does NOT calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## accepts a CacheMatrix and calculates it's inverse. If the
## inverse does not exist, it solves for it and caches the inverse.
## if the inverse exists, then it retrieves it from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)        	
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# create a matrix
my_matrix <- rbind(c(2, 3), c(3, 2))

# print the inverse
solve(my_matrix)

# initialize
c_m <- makeCacheMatrix(my_matrix)

# get the matrix
c_m$get()

# get the inverse (s/b NULL)
c_m$getinverse()

# solve for the inverse
cacheSolve(c_m)

# get the inverse
c_m$getinverse()
