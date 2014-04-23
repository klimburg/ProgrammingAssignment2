## The following two functions will cache a matrix and its inverse
## This is accomplished by creating a list of functions that set and get both 
## the matrix and its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      #set m to null
      m <- NULL
      #set function defined
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #return input matrix, x
      get <- function() x 
      #set m to inverse of x
      setinverse <- function(solve) m <<- solve  
      #null if cacheSolve hasn't been called yet, else return inverse x
      getinverse <- function() m
      #return a list containing the four functions above
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##cacheSolve takes a list created by cacheMatrix() and returns and caches
## an inverse matrix. If the inverse has already been computed it will retrieve
## it from the cache

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      
      #get data from cachedMatrix
      m <- x$getinverse()
      #if inverse has been calculated return inverse
      if(!is.null(m)) {
            message("getting cached data")
            return(m) #escape function by returning the inverse
      }
      #else get the matrix 
      data <- x$get()
      #calculate the inverse using solve
      m <- solve(data, ...)
      #set the inverse matrix of our cached matrix x
      x$setinverse(m)
      m
}
