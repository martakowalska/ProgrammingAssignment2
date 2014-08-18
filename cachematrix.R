## Functions below are designed to cache a matrix and its inverse in order to save time
## necessary for computing the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse. 
## The "matrix" object is really a list containing a functions to:
##1.  set the value of the matrix - function set
##2.  get the value of the matrix - function get
##3.  set the value of the matrix's inverse - function setinverse
##4.  get the value of the matrix's inverse - function getiverse


makeCacheMatrix <- function(x = matrix()) { ##argument x is a matrix
      inv <- NULL  ## matrix's inverse is set as a NULL every time when 
                   ## makeCacheMatrix is called
      set <- function(y) {    ## take the input matrix
            x <<- y     ## save the input matrix
            inv <<- NULL  ## set the inverse as a NULL
      }
      get <- function() {
            x ## get returns the value of the saved matrix
      }   
      setinverse <- function(solve) { ## this function is called by cacheSolve() during
                                    ## the first cacheSolve() access
            inv <<- solve ## stores matrix's inverse using superassignment
      }
      getinverse <- function() {
            inv  ## getinverse returns the value of the saved matrix's inverse to
      }          ## the cacheSolve() function      
      list(set = set, get = get,  ## this list allows access to the sub-functions 
           setinverse = setinverse, ## without it cacheSolve() won't work
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix 
## has not changed), then `cacheSolve` should retrieve the inverse from the cache and 
## print a message "getting cached inverse"

cacheSolve <- function(y, ...) { ## an argument is an object created by 
                                 ## makeCacheMatrix function

      inv <- y$getinverse() ## get value of object's y inverse
      
      if(!is.null(inv)){  ## if the inverse was already cached (so is not NULL)
              message("getting cached inverse") ## print message
              return(inv)     ## and return value of inverse
      } ## in addition in this case we know that the matrix hasn't change
        ## because each time when makeCacheMatrix is called, inv value is set to NULL
      
      data <- y$get()   ## this is done only if inverse is NULL
      inv <- solve(data, ...) ## so we create matrix's inverse
      y$setinverse(inv) ## and store it in object y as setinverse()
      inv ## returns value of matrix's inverse
}
