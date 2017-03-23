## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#mat is the argument that stores the information of the original matrix.
#Inv es the local variable able to store de inverse of the matrix 'mat'
#set is the local function that will permit setting the value of 'mat'
#get is the local function that will permit getting the actual value of 'mat'
#setInverts sets the inverted matrix of 'mat'
#getInverted gets the the inverted matrix of 'mat'


makeCacheMatrix <- function(mat = matrix()) {
       Inv <- NULL
       set <- function(y) {
              mat <<- y
              Inv <<- NULL
       }
       get <- function() mat
       setInverted <- function(solve) Inv <<- solve
       getInverted <- function() Inv
       list(set = set, get = get,
            setInverted = setInverted,
            getInverted = getInverted)
}


## Write a short comment describing this function

#First of all, the local variable Inv gets the value of the inverted matrix
#Then checks if the local variablew Inv is null, if it's not, then returns the value from the "cache"
#If it is null, then calculates ir and stores it in the cache.
#Finally, it returns the value of the inverted matrix

cacheSolve <- function(mat, ...) {
       Inv <- mat$getInverted()
       if(!is.null(Inv)) {
              message("getting cached data")
              return(Inv)
       }
       data <- mat$get()
       Inv <- solve(data, ...)
       mat$setInverted(Inv)
       Inv      
}

# > matriz <- matrix(c(2, 5, 1, 3), 2, 2)
              
# > r <- makeCacheMatrix()
              
# > r$set(matriz)
              
# > s <- cacheSolve(r)
              
# > s
# [,1] [,2]
# [1,]    3   -1
# [2,]   -5    2
              
# > s <- cacheSolve(r)
# getting cached data
              
# > s
# [,1] [,2]
# [1,]    3   -1
# [2,]   -5    2
