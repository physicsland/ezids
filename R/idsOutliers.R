#' Original outlierKD function by By Klodian Dhana,
#' https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
#' Modified to have third argument for removing outliers instead of interactive prompt,
#' and after removing outlier, original df will not be changed. The function returns the a df,
#' which can be saved as original df name if desired.
#' Also added QQ-plot in the output, with options to show/hide boxplot, histogram, qqplot.
#' Check outliers, and option to remove them, save as a new dataframe.
#' @param df The dataframe.
#' @param var The variable in the dataframe to be checked for outliers
#' @param rm Boolean. Whether to remove outliers or not.
#' @param boxplt Boolean. Whether to show the boxplot, before and after outliers removed.
#' @param histogram Boolean. Whether to show the histogram, before and after outliers removed.
#' @param qqplt Boolean. Whether to show the qqplot, before and after outliers removed.
#' @return The dataframe with outliers replaced by NA if rm==TRUE, or df if nothing changed
#' @examples
#'   outlierKD2(mydf, height, FALSE, TRUE, TRUE, TRUE)
#'   mydf = outlierKD2(mydf, height, TRUE, TRUE, TRUE, TRUE)
#'   mydfnew = outlierKD2(mydf, height, TRUE)
#' @export
outlierKD2 <- function(df, var, rm=FALSE, boxplt=FALSE, histogram=TRUE, qqplt=FALSE) {
  dt = df # duplicate the dataframe for potential alteration
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  colTotal <- boxplt+histogram+qqplt
  par(mfrow=c(2, max(2,colTotal)), oma=c(0,0,3,0)) # fixed issue with only 0 or 1 chart selected
  if (qqplt) {
    qqnorm(var_name, main = "With outliers")
    qqline(var_name)
  }
  if (histogram) { hist(var_name, main="With outliers", xlab=NA, ylab=NA) }
  if (boxplt) { boxplot(var_name, main="With outliers") }

  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  if (qqplt) {
    qqnorm(var_name, main = "Without outliers")
    qqline(var_name)
  }
  if (histogram) { hist(var_name, main="Without outliers", xlab=NA, ylab=NA) }
  if (boxplt) { boxplot(var_name, main="Without outliers") }

  if(colTotal > 0) {  # if no charts are wanted, skip this section
    title("Outlier Check", outer=TRUE)
    na2 <- sum(is.na(var_name))
    cat("Outliers identified:", na2 - na1, "\n")
    cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
    cat("Mean of the outliers:", round(mo, 2), "\n")
    m2 <- mean(var_name, na.rm = T)
    cat("Mean without removing outliers:", round(m1, 2), "\n")
    cat("Mean if we remove outliers:", round(m2, 2), "\n")
  }

  # response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  # if(response == "y" | response == "yes"){
  if(rm){
    dt[as.character(substitute(var))] <- invisible(var_name)
    #assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else {
    cat("Nothing changed", "\n")
    return(invisible(df))
  }
}


#' outlierKD function by By Klodian Dhana,
#' https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
#' @export
outlierKD <- function(dt, var) {
    var_name <- eval(substitute(var),eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(var_name, main="With outliers")
    hist(var_name, main="With outliers", xlab=NA, ylab=NA)
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
    title("Outlier Check", outer=TRUE)
    na2 <- sum(is.na(var_name))
    cat("Outliers identified:", na2 - na1, "\n")
    cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
    cat("Mean of the outliers:", round(mo, 2), "\n")
    m2 <- mean(var_name, na.rm = T)
    cat("Mean without removing outliers:", round(m1, 2), "\n")
    cat("Mean if we remove outliers:", round(m2, 2), "\n")
    response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
    if(response == "y" | response == "yes"){
        dt[as.character(substitute(var))] <- invisible(var_name)
        assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
        cat("Outliers successfully removed", "\n")
        return(invisible(dt))
    } else {
        cat("Nothing changed", "\n")
        return(invisible(var_name))
    }
}


