
##In the following code the <<- operator which can is used to assign a value to an 
##object in an environment that is different from the current environmentand uses the concept of closures 
##There  are two functions that are used to create a special object that
##stores a matrix and cache's its inverse 


##The first function, makeVector creates a special "matrix", which is really a list containing a function to 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the inverse of the matrix
##4.get the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
    set <- function(v,nr,nc) {
##v vector of elements,nr,nc are the dimensions which should be same(square matrix)
      x<<- matrix(v,nr,nc)
            m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the mean of the special "matrix" 
##created with the above function. However, it first checks to see 
##if the inverse has already been calculated. If so, it gets it 
##from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of
##the inverse of the matrix in the cache via the setsolve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
