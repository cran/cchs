################################################################################
# Miscellaneous subroutines. 

################################################################################
# Find whether all elements of a vector are equal. 
allEqual <- function(x) {
	if (!is.vector(x)) stop("x must be a vector") 
	isTRUE(all.equal(x, rep(x[1], length(x))))
}

# Find the smallest difference between any pair of values in a vector. 
minDifference <- function(x, allowZeroResult=FALSE) {  
	differences <- c(sort(x)[-1]) - c(sort(x)[-length(x)])
	if (allowZeroResult) {
		return(min(differences))
	} else {
		return(min(differences[differences!=0]))
	}
} 

# Find whether a factor has any unused levels.
hasUnusedLevels <- function(x) {
	if (!is.factor(x)) stop("x must be a factor")
	!identical(sort(levels(x)), sort(levels(droplevels(x))))
}

# Remove NAs from a vector. 
removeNAsFromVector <- function(x) {
	if (!is.vector(x)) stop("x must be a vector")
	as.vector(na.omit(x))  # or x[!is.na(x)]
}

################################################################################

