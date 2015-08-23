## defining function makeCacheMatrix to accept matrix x as input 
makeCacheMatrix <- function(x = matrix()) 
{ 
  ## setting up variable to store inverse matrix
  v_inv <- NULL 
  ## function to set value of matrix  
  set <- function(y) 
  { 
    ## assigning value to cached environment
    x <<- y 
    v_inv <<- NULL 
  } 
  ## function to get value of matrix    
  get <- function() x 
  ## function to set value of inverse of matrix
  setinverse <- function(inverse) v_inv <<- inverse 
  ## function to get value of inverse of matric
  getinverse <- function() v_inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 

## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cach 

cacheSolve <- function(x, ...) { 
  v_inv <- x$getinverse() 
  ##check is matrix inverse is present in cache, if yes return.
  if(!is.null(v_inv)) { 
    message("Retrieving data from cache.") 
    return(v_inv) 
  } 
  ##if matrix inverse is not calculated the calculate it and set it in cached environment
  data <- x$get() 
  v_inv <- solve(data) 
  x$setinverse(v_inv) 
  v_inv 
} 

