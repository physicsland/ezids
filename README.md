# Ezids Package 

An R package providing easy to use formatting and other helper functions. 
This package is created for students enrolled in an Introductory Data Science 
courses (DATS-1001 / DATS-6101) at The George Washington University.

## Contributors

+ Dr. Edwin Lo (George Washington University)
+ Dr. Divya Pandove (George Washington University) 

### Community contributors

+ Mark Febrizio

### Acknowledgments

The outlier function here is modified from the outlierKD function authored by
[Klodian Dhana](https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/).


## Dependencies 

The packages listed below are needed to run ezids. They will be installed as dependencies:  
faraway, xtable, kableExtra, stringr, svglite  


## Install

Install the development version from Github:


### Install remotes if not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

### Install devtools if not already 
install.packages ("devtools)


### Install ezids from github
devtools::install_github("physicsland/ezids")

## Authorization

MIT License

Copyright (c) 2021 Edwin Lo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


## Change Log

+ 2022-07-08 Modified xkablevif() function with dynamic default title
+ 2022-05-24 Added api_rfit() function
+ 2022-05-05 Various minor fixes by mfebrizio






