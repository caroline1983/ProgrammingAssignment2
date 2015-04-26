  #This function creates a special "matrix" object
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #that can cache its inverse.
    set <- function(y) {
      x <<- y    # Set the value
      m <<- NULL 
    }
    # Define function to get the value of the matrix
    # Define function to get the inverse
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m 
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


  #'  function computes the inverse of the special "matrix" returned 
  #'  by makeCacheMatrix above. If the inverse has already been calculated 
  #'  (and the matrix has not changed), then the cachesolve should retrieve
  #'   the inverse from the cache.
  
  cacheSolve <- function(x) {
    m <- x$getInverse() # This fetches the cached value for the inverse
    if(!is.null(m)) { # If the cache was not empty, we can just return it
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()  # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m)  # Return the inverse
    m                
  }
  