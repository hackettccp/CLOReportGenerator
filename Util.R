sdrange <- function(x, n, m, s) {
  one <- dnorm(x, m, s)
  one[x<=m-s*n | x>= m+s*n] <- NA
  return(one)
}

lefttail <- function(x, t, m, s) {
  left <- dnorm(x, m, s)
  left[x>t] <- NA
  return(left)
}

righttail <- function(x, t, m, s) {
  right <- dnorm(x, m, s)
  right[x<t] <- NA
  return(right)
}

