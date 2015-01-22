## The following functions allow a user to compute inverses of matrices 
## in an efficient way.An inverse is actually computed only if it was never
## computed before.Otherwise, a pre-computed inverse of the matrix is returned, thus
## precluding unneccessary computation.

## Creates and returns a special matrix, which is actually a list
##containing functions to:
## set the value of a matrix
## get the value of a matrix
## set the inverse of that matrix
## get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##inverse i of matrix x
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }##follow pointer to enclosing env and 
  #change the values of i and x there
  ##i.e. empty the cache(value of inverse in env of makeCacheMatrix)
  ##and reset the matrix to the provided value
  
  get <- function() x##follow pointer to enclosing env and
  #get the value of x there
  
  setinverse <- function(Inverse) i <<- Inverse ##follow pointer to enclosing env and 
  #change the value of i there
  
  getinverse <- function() i##follow pointer to enclosing env and
  #get the value of i there
  
  ##the set functions thus store or cache a value in their enclosing environment
  ##the get functions thus retrieve the cached values from the enclosing environment,
  ##which they share with set functions.
  
  ##return a list of environments associated
  ##with functions so created
  
  ##NOTE: every env in the list will carry with it a pointer 
  ##to its enclosing env.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function returns inverse of a give matrix.
##However, it first checks a cache to see if the inverse
##is already present there.If it is, it returns that cached value.
##Otherwise, it computes the inverse, stores it in the cache for
##future reference and returns the value so computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x here is a list on environments
  
  inverse <- x$getinverse()## invoke the getinverse environment to access the i variable
  ##recessed in its enclosing environment
  
  if(!is.null(inverse)){ 
    ##if the procured value is not null, then that
    ##pre-computed value can be returned
    
    message("getting cached data")
    return(inverse)
    
  }
  ##otherwise, compute the inverse and store it using setinverse function
  
  data <- x$get() ## invoke the get environment to access the matrix recessed in 
  ##its enclosing environment
  
  ##Assumption: data is an invertible matrix
  computedInverse <- solve(data) ## compute inverse of the matrix
  
  x$setinverse(computedInverse) ## lay it inside enclosing environment
  ## of set and get functions (they share the same enclosing environment)
  ## so that next time, instead of computing, cacheSolve will retrieve
  ##the value from that storage(the aforementioned enclosing environment)
  
  computedInverse
  ##return the inverse so computed
  
}
##Examples/Tests
##>m3 <- makeCacheMatrix(rbind(c(4,7),c(2,6)))
##>cacheSolve(m3) ##computes the inverse
##>cacheSolve(m3) ##uses the cached copy

##>m4 <- makeCacheMatrix(rbind(c(4,7),c(2,6)))
##>cacheSolve(m4) ##computes the inverse because m4 and m3
##have different enclosing envs(different storage that is)
##>cacheSolve(m4) ##uses the cached copy

##> m3 <- makeCacheMatrix(rbind(c(4,7),c(2,6)))
##> cacheSolve(m3) ##computes the inverse
##>cacheSolve(m3) ##uses the cached copy
##m3$set(rbind(c(2,0,0),c(0,2,0),c(0,0,2)))##set m3 to another matrix
##>cacheSolve(m3) ##computes the inverse
##>cacheSolve(m3) ##uses the cached copy
##Enclosing environment before and after resetting matrix is the same