# making these functions as a package
# https://swcarpentry.github.io/r-novice-inflammation/08-making-packages-R/
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# v 0.1.2020.11


#' Combining base::summary, xtable, and kableExtra, to easily display model summary.
#' wrapper for the base::summary function on model objects
#' Can also use as head/tail for nicer display
#' ELo 202004 GWU DATS
#' version 1.2.2
#' @param modelsmmrytable This can be a generic table, a model object such as lm(), or the summary of a model object summary(lm())
#' @param title Title of table. If modelsmmrytable has a "formula", it will be the default title
#' @param digits Number of digits to display
#' @param pos Position of table, c("left","center","right")
#' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
#' @param wide print table in long (FALSE) format or wide (TRUE) format
#' @return HTML table for display
#' @examples
#' xkabledply( df, title="Table testing", pos="left", bso="hover" )
#' xkabledply( ISLR::Hitters[1:5,] )
#' @export
xkabledply <- function(modelsmmrytable, title="", digits = 4, pos="left", bso="striped", wide=FALSE) {
  wtitle = stringr::str_trim(title) # working title
  if (wtitle=="") {
    trytitle <- tryCatch(
      {
        tmp <- formula(modelsmmrytable) # typically works for model objects and summary(model) objects
        wtitle <- paste("Model:", format(tmp) )
      },
      error=function(cond){ return("no-formula-error") }
    )

    if (trytitle == "no-formula-error") { wtitle <- "Table" }
  }
  if (wide) { modelsmmrytable <- t(modelsmmrytable) }
  kableExtra::kable_styling( kableExtra::kable( xtable::xtable( modelsmmrytable ) , caption = wtitle, digits = digits) , bootstrap_options = bso, full_width = FALSE, position = pos)
}

#' Better display than the default head() function
#' ELo 202004 GWU DATS
#' @param df Dataframe
#' @param title Title of table.
#' @param digits Number of digits to display
#' @param pos Position of table, c("left","center","right")
#' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
#' @return HTML table for display
#' @examples xkabledplyhead( ISLR::Hitters[1:5,], title="Head of df", pos="left", bso="hover" )
#' @export
xkabledplyhead <- function(df, rows=5, title="Head", digits = 4, pos="left", bso="striped") {
  xkabledply(df[1:rows, ], title, digits, pos, bso, wide=FALSE)
}

#' Better display than the default tail() function
#' ELo 202004 GWU DATS
#' @param df Dataframe
#' @param title Title of table.
#' @param digits Number of digits to display
#' @param pos Position of table, c("left","center","right")
#' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
#' @return HTML table for display
#' @examples xkabledplytail( ISLR::Hitters[1:5,], title="Tail of df", pos="left", bso="hover" )
#' @export
xkabledplytail <- function(df, rows=5, title="Tail", digits = 4, pos="left", bso="striped") {
  trows = nrow(df)
  xkabledply(df[ (trows-rows+1) : trows, ], title, digits, pos, bso, wide=FALSE)
}

#' Combining base::summary, xtable, and kableExtra, to easily display numeric variable summary of dataframes.
#' ELo 202004 GWU DATS
#' version 1.2
#' @param df The dataframe.
#' @param title Title of table.
#' @param digits Number of digits to display
#' @param pos Position of table, c("left","center","right")
#' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
#' @return The HTML summary table for display, or for knitr to process into other formats
#' @examples
#'   xkablesummary( ISLR::Hitters, title="Five number summary", pos="left", bso="hover"  )
#' @export
xkablesummary <- function(df, title="Table: Statistics summary.", digits = 4, pos="left", bso="striped") {
  s = summary(df)
  apply( s, 2, function(x) stringr::str_remove_all(x,c("Min.\\s*:\\s*","1st Qu.\\s*:\\s*","Median\\s*:\\s*","Mean\\s*:\\s*","3rd Qu.\\s*:\\s*","Max.\\s*:\\s*")) ) # %>% # replace all leading words
  apply( s, 2, function(x) stringr::str_trim(x, "right")) # trim trailing spaces left

  colnames(s) <- stringr::str_trim(colnames(s))

  if ( dim(s)[1] ==6 ) { rownames(s) <- c('Min','Q1','Median','Mean','Q3','Max')
  } else if ( dim(s)[1] ==7 ) { rownames(s) <- c('Min','Q1','Median','Mean','Q3','Max','NA') }

  xkabledply(s, title=title, digits = digits, pos=pos, bso=bso )
}

#' Combining faraway::vif, xtable, and kableExtra, to easily display numeric summary of VIFs for a model.
#' ELo 202004 GWU DATS
#' version 1.2
#' @param model The lm or compatible model object.
#' @param title Title of table.
#' @param digits Number of digits to display
#' @param pos Position of table, c("left","center","right")
#' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
#' @param wide print table in long (FALSE) format or wide (TRUE) format
#' @return The HTML summary table of the VIFs for a model for display, or for knitr to process into other formats
#' @examples xkablevif( lm(Salary~Hits+RBI, data=ISLR::Hitters), wide=T )
#' @export
xkablevif <- function(model, title="VIFs of the model", digits = 3, pos="left", bso="striped", wide=TRUE) {
  vifs = table( names(model$coefficients)[2:length(model$coefficients)] ) # remove intercept to set column names
  vif_res = faraway::vif(model) # calculate vifs
  vifs[] = vif_res[order(names(vif_res))] # set the values after sorting by variable names
  if (wide) { vifs <- t(vifs) }
  xkabledply( vifs, title=title, digits = digits, pos=pos, bso=bso )
}

