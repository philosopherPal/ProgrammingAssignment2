## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function will create a matrix object to cache the inverse of a matrix. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL;
	set <- function(y)
		{
		x <<- y;
		inv <<- NULL;
		}
	get <- function() x
	set_Inv <- function(inverse) inv <<- inverse;
	get_Inv <- function() inv
	list(set = set,
	     get = get,
	     set_Inv = set_Inv,
	     get_Inv = get_Inv);

}


## Write a short comment describing this function
## cacheSolve function will compute the inverse of the matrix that has been created in the above function. 
##If the inverse has already been computed for the same matrix it will retrieve the cached response. 
## Otherwise, It will compute the inverse of the matrix.
## The cost of Computation may be benifited by the Cached response as inversion of a matrix is computationally costly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_Inv();
	if(!is.null(inv))
		{
		message("getting cached data");
		return(inv);
		}
	mat_input <- x$get();
	inv <- solve(mat_input, ...);
	x$set_Inv(inv);
	inv;

}
## Runtime Summary.......................................................................................................................
##source("cachematrix.R")
##> my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
##> my_matrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> my_matrix$get_Inv()
##NULL
##> cacheSolve(my_matrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(my_matrix)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> my_matrix$get_Inv()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> my_matrix$set(matrix(c(2,2,1,4),2,2))
##> my_matrix$get()
##     [,1] [,2]
##[1,]    2    1
##[2,]    2    4
##> my_matrix$get_Inv()
##NULL
##> cacheSolve(my_matrix)
##           [,1]       [,2]
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
##> cacheSolve(my_matrix)
##getting cached data
##           [,1]       [,2]
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
##> my_matrix$get_Inv()
##           [,1]       [,2]
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
