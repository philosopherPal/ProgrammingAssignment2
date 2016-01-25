## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
