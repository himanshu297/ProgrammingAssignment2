						# make a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {
  
						# initialize the cache Matrix 'cacheMatrix'
						# assign the value NULL for the first initialization
  
  cacheMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
    
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  
                                                 # Define the Function named 'getCache' that will return the cached inverse of 'x'
    
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
}


cacheSolve <- function(x, ...) {
  
                                                  # 'cacheSolve' return the inverse of a given matrix utilizing the cache
  
   cacheMatrix <- x$getCache()
  
   
  
  if (!is.null(cacheMatrix)) {                    # if the content is not null then: return the result
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  
 
  else {                                           # if the content is Null then get the matrix, create, set, update and return the cache matrix
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
 
}
