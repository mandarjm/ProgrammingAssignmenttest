#defining function makeCacheMatrix to accept matrix x as input 
makeCacheMatrix <- function(x = matrix()) 
{ 
  #setting up variable to store inverse matrix
  v_inv <- NULL 
  #function to set value of matrix  
  set <- function(y) 
  { 
    x <<- y 
    v_inv <<- NULL 
  } 
  #function to get value of matrix    
  get <- function() x 
  #function to set value of inverse of matrix
  setinverse <- function(inverse) v_inv <<- inverse 
  #function to get value of inverse of matric
  getinverse <- function() v_inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


cacheSolve <- function(x, ...) { 
  v_inv <- x$getinverse() 
  if(!is.null(v_inv)) { 
    message("Retrieving data from cache.") 
    return(v_inv) 
  } 
  data <- x$get() 
  v_inv <- solve(data) 
  x$setinverse(v_inv) 
  v_inv 
} 

