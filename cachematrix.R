#RProgramming Course Assignment2 Week 3

#makeCacheMatrix: creates special matrix object, which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set-method for matrix value (also set i to NULL)
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  #get-method for matrix value, returns m
  get <- function() {
    m
  }
  
  #set-method for inverse value
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #get-method for inverse value, returns i
  getInverse <- function() {
    i
  }

}


#cacheSolve: returns the inverse of the special matrix , if the inverse has already
#been calculated retrieve value from cache and return it
cacheSolve <- function(x, ...) {

  
  #retrieve inverse if it has already been set, otherwise retrives NULL
  i <- x$getInverse()
  
  #return inverse if it has already been set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  
  #get matrix
  m <- x$get()
  
  
  #calculate inverse through matrix multiplication 
  i <- solve(m) %*% m
  
  #set inverse of the matrix
  x$setInverse(i)
  
  #returns inverse of matrix
  i
}
