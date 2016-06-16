## "makeCacheMatrix " makes a list having four functions. 
## 1.set the value of a matrix 2.get the value of the matrix 
## 3.set the vaule of inversion matrix 4. get the value of inversion matrix

makeCacheMatrix <- function(x = matrix()) {
s<-NULL
	set<-function(y){	
	x<<-y
	s<<-NULL
	}
	get<-function()x
	setsolve<-function(solve) s<<-solve
	getsolve<-function()s
	list(set=set,get=get,
	setsolve=setsolve,
	getsolve=getsolve)
}


## "cahceSolve" transform the value of a matrix into a matrix that is inversed.
## Before computing the matrix 'x', it checks whether there are already computed value of the matrix data for efficiency. 
## if there are, It just returns the computed data instead of transforming again.
cacheSolve <- function(x, ...) {
       s<-x$getsolve()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data<-x$get()
	s<-solve(data,...)
	x$setsolve(s)
	s
}
