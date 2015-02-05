## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#PRE-CONDITION: x is the matrix where we want to convert to this special cache matrix "type". 
#POST-CONDITION: return the cache matrix. It will contain methods to set and get inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  #1. Define a variable to store the inverse matrix
  inv <- NULL #this is the inverse matrix
  
  #2. Set function for the matrix itself
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #3. Get the matrix
  get <- function() {
    return(x)
  }
  
  #4. Set the inverse matrix
  setinv <- function(inverse){
    inv <<- inverse
  }
  
  #5. Get the inverse matrix
  getinv <- function(){
    return(inv)
  }
  
  #6. Set the prototype functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
#PRE-CONDITION: x is the special cache matrix type 
#POST-CONDITION: return the inverse matrix. If the inverse matrix is cached, 
#                the cache inverse matrix will return. Else it will be computed then returned.
cacheSolve <- function(x, ...) {
  #1. Try to get the inverse matrix
  inv <- x$getinv()
  
  #2. If inverse matrix is cached (i.e. not null), we just return the inverse matrix
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  
  #3. Else if inverse matrix is not cache, we compute it then set the inverse matrix
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  return(inv)
   
}
