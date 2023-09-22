

#' pythagorean
#'
#' @param a a side leg of a right triangle
#' @param b a side leg of a right triangle
#' @param c hypotenuse of a right triangle
#'
#' @return The length of the rest side or hypotenuse
#' @export
#'
#' @examples
#' pythagorean(a=3,b=4)
pythagorean=function(a,b,c){
  if (!missing(a) & !missing(b) & !missing(c)) {
    stop("Provide two values only")
  }

  else if (missing(a)) {
    if(!is.numeric(b) | !is.numeric(c)){
      stop("An argument must be numeric ")
    }
    else{
      return(sqrt(c^2 - b^2))
    }

  }

  else if (missing(b)) {
    if(!is.numeric(a) | !is.numeric(c)){
      stop("An argument must be numeric ")
    }
    else{
      return(sqrt(c^2 - a^2))
    }

  }

  else if (missing(c)) {
    if(!is.numeric(a) | !is.numeric(b)){
      stop("An argument must be numeric ")
    }
    else{
      return(sqrt(a^2 + b^2))
    }

  }
}


#' trimmed_mean
#'
#' @param x A vector of numeric values
#' @param s The s smallest values to cut off
#' @param l The l largest values to cut off
#'
#' @return The average of a trimmed vector x
#' @export
#'
#' @examples
#' x<-c(1,7,3,2,5,0.5,9,10)
#' trimmed_mean(x,s=1,l=2)
#' ## trimmed vector with s=1,l=2 is c(1,2,3,5,7)
trimmed_mean=function(x,s,l){
  len=length(x)
  if(len < s+l+1){
    stop(paste("Input values must has at least", s+l+1,"values"))
  }
  else{
    sorted_x=sort(x)
    trimmed_x=sorted_x[-c(1:s,len:(len-l+1))]
    average=mean(trimmed_x)
    return(average)
  }
}




