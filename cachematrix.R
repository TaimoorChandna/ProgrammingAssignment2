##  library(MASS) is used to calculate inverse for non squared as well as square matrices 
library(MASS)

##  makeCacheMatrix consists of set, get, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL           # initializing inverse as NULL
  set <- function(y){
                    x<<-y
                    inv<<-NULL
                    }
  get <- function()x      # function to get matrix x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function(){
                      inver<-ginv(x)
                      inver%*%x     # function to obtain the inverse of matrix x
                      }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is used to get the cache data 

cacheSolve <- function(x, ...) # gets cache data 
{ 
  inv <- x$getinv()
  if(!is.null(inv)){            # checking whether inverse is NULL
                    message("Getting cached data!")
                    return(inv)         # returns inverse value
                    }
    
  data <- x$get()
  inv <- solve(data,...)        # calculates inverse value
  x$setinv(inv)
  inv                 ## Return a matrix that is the inverse of matrix x 
}
