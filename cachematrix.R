# This is r programming assignment 2

# function to create a test square matrix where number is the dimension
createx <- function(number){
      set.seed(1)
      createx <- matrix(rnorm(number*number),number,number)
      createx
}
# funtion for matrix equality test
# for TRUE both arguments must be a matrix, matrix must be same size and the elements must match
ismatequal <- function(x, y){
      is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) 
}
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      setmatrix = function(y) {     # set the value of the matrix
            m <<- y                 # caches the inputted matrix 
            inv <<- NULL            # resets the value of the matrix inverse 
      }      
      getmatrix = function(){
             x
      }
      setinverse = function(x){
             inv <<- solve(x)
      }      
      getinverse = function(x){
            inv
      }
      list(setmatrix = setmatrix,
           getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
      }

# 
# 
# # cacheSolve: This function computes the inverse of the special "matrix" returned by 
# # makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
# # changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function (x, ...) {
      
      inv <- x$getinverse()                     # get current value of inv
      if((!is.null(inv))) {                     # if an inverse has already been calculated
           if(ismatequal(m, x$getmatrix())) {   # and the matrix is the same
                  message("getting cached data")
                  return(inv)                   # use cache and exit function
            }
      }
      # otherwise calculate a new inversion
            message("getting new inv")
            y <- x$getmatrix()      # get the value of the input matrix
            x$setmatrix(y)          # cache it
            inv <- solve(y, ...)    # compute inverse of the input matrix
            x$setinverse(inv)       # cache the inverse
            inv                     # return the inverse
      }

test = function(mat){ # mat is the matrix dimension
      # sample call:  test(500)
      mat<-createx(mat)             # make random matxmat matrix 
      
      temp = makeCacheMatrix(mat)   # kick start the cache function

      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
}
