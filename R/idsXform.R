#' Universal z-scale function
#' Standardize dataframe to z scores, safe for non-numeric variables.
#' ELo 201904 GWU DATS
#' @param df The dataframe.
#' @param append T/F or 0/1. Option to append scaled columns or replace original columns in the dataframe.
#' @param excl A list c(a,b,"d","ef") of excluded columns, either by their indexes and/or names.
#' @return The transformed dataframe, appended or replaced with standardized scores. Non-numeric columns will not be appended, or if "replace option" is chosen, the columns will be untouched.
#' @examples
#'   tmp = uzscale( ISLR::Hitters )
#'   tmp = uzscale( ISLR::Hitters, 1 )
#'   tmp = uzscale( ISLR::Hitters, TRUE, c(19,"NewLeague") )
#' @export
uzscale <- function(df, append=0, excl=NULL) {
  append = ifelse(append==TRUE || append=="true" || append=="True" || append=="T" || append=="t" || append==1 || append=="1", TRUE, FALSE) # standardize append
  nmax = length(df)
  if (nmax < 1 || !is.numeric(nmax) ) { return(df) }
  df1 = df
  onames = colnames(df)  # the original column names
  cnames = onames  # the new column names, if needed start with the original ones
  znames = paste("z",cnames, sep="")     # new column names added prefix 'z'. Those are non-numeric will not be used.
  nadd = ifelse(append, nmax, 0) # add to the column index or replace the orig columns
  j=1  # counting index
  for( i in 1:nmax ) {
    if ( is.numeric(df[,i]) && !( i %in% excl || onames[i] %in% excl ) ) {
      df1[,j+nadd] = scale(df[,i])
      cnames = c(cnames, znames[i])
      j=j+1
    } else if ( !append ) { j=j+1
    } # if append == 1 and (colunm non-numeric or excluded), do not advance j.
  }
  if (append) { colnames(df1) <- cnames }
  return(df1)
}
# sample
# HittersClean = subset(ISLR::Hitters, Salary != "NA")
# tmp = uzscale(HittersClean,1,c(2,"Salary") )



#' PCA transform function
#' Obtain the dataframe with the Principal Components after the rotation.
#' ELo 201911 GWU DATS
#' @param df The dataframe.
#' @param z T/F or 0/1 for z-score to be used
#' @return The transformed dataframe.
#' @examples tmp = PCAxform(USArrests,TRUE)
#' @export
PCAxform <- function(df, z=TRUE) {

  z = ifelse(z==TRUE || z=="true" || z=="True" || z=="T" || z=="t" || z==1 || z=="1", TRUE, FALSE) # standardize z
  if(z) { df = data.frame(scale(df))}  # scale not safe for non-numeric colunms, but PCA requires all variables numerics to begin with.
  pr.out = prcomp(df,scale=z)
  df1 = data.frame( as.matrix(df) %*% pr.out$rotation ) # use matrix multiplication in R:  %*%
  return(df1)
}
# Sample
# USArrests.z.pc = PCAxform(USArrests,TRUE)
# summary(USArrests.z.pc)


#' Obtain the dataframe with the Principal Components after the rotation for PCRegression. Requires related function PCAxform()
#' ELo 201903 GWU DATS
#' @param df The dataframe.
#' @param y The y-variable column index number(int), or the name of y-variable
#' @param zX T/F or 0/1 for z-score used on X-variables
#' @param zy T/F or 0/1 for z-score used on the target y-variable
#' @return The transformed dataframe.
#' @examples tmp = PCAxform(USArrests,TRUE)
#' @export
PCRxform <- function(df, y, zX=TRUE, zy=FALSE) {

  # take care of y target
  zy = ifelse(zy==TRUE || zy=="true" || zy=="True" || zy=="T" || zy=="t" || zy==1 || zy=="1", TRUE, FALSE) # standardize target y
  if( is.integer(y) ) { # y is integer
    if( y>length(df) || y<1 ) {
      print("Invalid column number")
      return(NULL)
    }
    if(zy) { df1 = data.frame( scale(df[y]) ) } else { df1 = df[y] } # save y-var in df1
    df = df[-y] # remove y-variable in df
  } else { # y is not integer, so interpret as name
    if(zy) { df1 = data.frame( scale( df[names(df) == y] ) ) } else { df1 = df[names(df) == y] }
    df = df[names(df) != y] # remove y-variable in df
  }
  if( length(df1)<1 ) {
    print("Variable name not found in data.frame")
    return(NULL)
  }
  # now transform X-vars
  zX = ifelse(zX==TRUE || zX=="true" || zX=="True" || zX=="T" || zX=="t" || zX==1 || zX=="1", TRUE, FALSE) # standardize X-vars
  df2 = PCAxform(df,zX)
  df1 = data.frame(df1,df2) # piece them back together
  return(df1)
}
# Sample
# USArrests.z.pcr = PCRxform(USArrests,3,TRUE) # OR
# USArrests.z.pcr = PCRxform(USArrests,"UrbanPop",TRUE)
# summary(USArrests.z.pcr)


