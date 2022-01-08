## The first function creates a special matrix which contains a list of functions 1. Set the matrix 2. Get the matrix 3. Set the inverse 4.Get the inverse
##The second function computes the inverse. If inverse already exists, it skips, else it returns the computed inverse

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##initialize inv as NULL
  
            set <- function(y) {  ##define function to assign matrix 
              x <<- y
              inv <<- NULL       
            }
                  get <- function() x  ## get value of matrix
                  setinverse <- function(inverse) inv <<- inverse  ##assign value of inverse
                  getinverse <- function() inv ##get value of inverse
                  
                  list(set = set, get = get, ##return list
                       setinverse = setinverse,
                       getinverse = getinverse)

}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse() 
          if(!is.null(inv)) {                    
            message("getting cached data")
            return(inv)
          }
          data <- x$getinverse()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
}
