## ----setup, echo = FALSE, warning = FALSE, message = FALSE---------------

library(knitr)
library(dplyr)
library(huxtable)

is_latex <- knitr::opts_knit$get('rmarkdown.pandoc.to') == 'latex'
# is_latex <- TRUE
knitr::knit_hooks$set(
  barrier = function(before, options, envir) {
    if (! before && is_latex) knitr::asis_output('\\FloatBarrier')
  }
)

# knitr::opts_chunk$set(results = 'asis')


## ----echo = FALSE--------------------------------------------------------
huxtable::hux_logo(latex = is_latex)

## ------------------------------------------------------------------------
library(huxtable)
ht <- hux(
        Employee = c('John Smith', 'Jane Doe', 'David Hugh-Jones'), 
        Salary = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

## ------------------------------------------------------------------------
data(mtcars)
car_ht <- as_hux(mtcars)

## ---- results = 'markup'-------------------------------------------------
print(ht)

## ------------------------------------------------------------------------
ht

## ------------------------------------------------------------------------

bold(ht)[1,]           <- TRUE
bottom_border(ht)[1,]  <- TRUE
align(ht)[,2]        <- 'right'
right_padding(ht)      <- 10
left_padding(ht)       <- 10

ht

## ------------------------------------------------------------------------

caption(ht) <- 'Employee table'

ht


## ------------------------------------------------------------------------

# First do library(magrittr) or library(dplyr):
ht %>% 
      set_bold(1, 1:2, TRUE)          %>% 
      set_bottom_border(1, 1:2, 1)    %>%
      set_align(-1, 2, 'right')       %>%
      set_right_padding(1:4, 1:2, 10) %>%
      set_left_padding(1:4, 1:2, 10)
  


## ---- results = 'markup'-------------------------------------------------
italic(ht)
position(ht)
bottom_border(ht)[1:2,] # first two rows

## ------------------------------------------------------------------------
number_format(car_ht) <- 0
add_colnames(car_ht[1:5,])

## ------------------------------------------------------------------------
col_width(ht) <- c('30pt', '40pt')
ht

## ------------------------------------------------------------------------
ht[4, 1] <- 'David Arthur Shrimpton Hugh-Jones'
ht 

## ------------------------------------------------------------------------
ht_wrapped <- ht
wrap(ht_wrapped) <- TRUE
ht_wrapped

## ------------------------------------------------------------------------


cars_mpg <- car_ht[, c('mpg', 'cyl', 'am')]
cars_mpg <- cars_mpg[order(cars_mpg$cyl),]

cars_mpg <- cars_mpg %>% 
      huxtable::add_rownames(colname = 'Car name') %>% 
      huxtable::add_colnames()

cars_mpg[1:5,]

## ------------------------------------------------------------------------
cars_mpg <- cbind(car_type = rep("", nrow(cars_mpg)), cars_mpg)
cars_mpg$car_type[1] <- 'Four cylinders'
cars_mpg$car_type[13] <- 'Six cylinders'
cars_mpg$car_type[20] <- 'Eight cylinders'
rowspan(cars_mpg)[1, 1] <- 12
rowspan(cars_mpg)[13, 1] <- 7
rowspan(cars_mpg)[20, 1] <- 14

cars_mpg <- rbind(c('', 'List of cars', '', '', ''), cars_mpg)
colspan(cars_mpg)[1, 2] <- 4
align(cars_mpg)[1, 2] <- 'center'

# a little more formatting:

cars_mpg <- set_all_padding(cars_mpg, , , 2)
cars_mpg <- set_all_borders(cars_mpg, , , 1)
valign(cars_mpg)[1,] <- 'top'
col_width(cars_mpg) <- c(.4 , .3 , .1, .1, .1)

if (is_latex) font_size(cars_mpg) <- 10
cars_mpg

## ------------------------------------------------------------------------
theme_striped(cars_mpg[14:20,], stripe = 'bisque1', header_col = FALSE, header_row = FALSE)

## ---- results = 'markup'-------------------------------------------------

print_screen(ht)

