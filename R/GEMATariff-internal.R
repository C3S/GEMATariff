# Copyright 2016 Meik Michalke <meik.michalke@c3s.cc>
#
# This file is part of the R package GEMATariff.
#
# GEMATariff is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GEMATariff is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with GEMATariff.  If not, see <http://www.gnu.org/licenses/>.

## function rnd2()
# shortcut to round to two digits
rnd2 <- function(value){
  return(round(x=value, digits=2))
} ## end function rnd2()


## function reduction_period()
# reduction factor for contract periods
reduction_period <- function(period){
  reductionFactor <- 1 - switch(period,
    m=0,
    q=1/12, # 0.0833
    y=1/6,  # 0.1667
    stop(simpleError("Wrong period, must be 'm', 'q' or 'y'!"))
  )
  return(reductionFactor)
} ## end function reduction_period()


## function factor_period()
# what to multiply monthly tariff by
factor_period <- function(period){
  periodFactor <- switch(period,
    m=1,
    q=3,
    y=12,
    stop(simpleError("Wrong period, must be 'm', 'q' or 'y'!"))
  )
  return(periodFactor)
} ## end function factor_period()


## function category_steps()
# find out a category of something,
# minimum is always 1. or the number of the largest value in 'steps'
# that 'value' includes
# div: if TRUE will take 'steps' to divide 'value'
# names: if not NULL, returns the nth name instead of the number.
#   length must be length of 'steps' plus 1!
category_steps <- function(value, steps, div=FALSE, names=NULL){
  if(isTRUE(div)){
    result <- max(1, ceiling(value / steps))
  } else {
    result <- max(1, max(which(value > steps) + 1, rm.na=TRUE))
    if(!is.null(names)){
      stopifnot(length(names) >= result)
      result <- names[result]
    } else {}
  }
  return(result)
} ## end function category_steps()


## function category_space()
# find out the price category by room size
category_space <- function(space){
  return(category_steps(value=space, steps=100, div=TRUE))
} ## end function category_space()

#     2015  2016  2017  2018  2019  2020
#     4.00  4.40  4.80  5.20  5.60  6.00

## function reduction_entry()
# market entry reduction factor
# values can be rising or falling, depending on how
# they are used in their respective methods
reduction_entry <- function(year, tariff="m_cd"){
  stopifnot(is.numeric(year))
  entryReduction <- switch(tariff,
                     # 2013   2014   2015     2016     2017     2018     2019     2020     2021     2022
    "m_cd"=          c(   0,    80,    75,      65,      60,      60,      50,      35,      20,       0),
    "u_k_le2000"=    c(   0,     0,     5,       6,       7,       8,       9,      10,      10,      10),
    "u_k_gt2000"=    c(   0,     0,   7.2,    7.65,    8.24,    8.83,    9.42,      10,      10,      10),
    "u_k_addIncome"= c(   0,     0,  0.35,     0.6,     0.7,     0.8,     0.9,       1,       1,       1),
    "u_k_promoYoung"=c(   0,     0,  4.00,    4.40,    4.80,    5.20,    5.60,    6.00,    6.00,    6.00),
    stop(simpleError(paste0("Unknown tariff: ", tariff)))
  )
  if(year > 2021){
    reductionFactorEntry <- entryReduction[length(entryReduction)] / 100
  } else if(year > 2012){
    reductionFactorEntry <- entryReduction[year - 2012] / 100
  } else {
    reductionFactorEntry <- 0
  }
  return(reductionFactorEntry)
} ## end function reduction_entry()


## function min_price()
min_price <- function(year, tariff="u_k_each150"){
  stopifnot(is.numeric(year))
  minPriceSwitch <- switch(tariff,
                     # 2014   2015     2016     2017     2018     2019     2020     2021
    "u_k_each150"=   c(   0, 22.80,   23.05,   23.30,   23.55,   23.55,   23.55,   23.55),
    stop(simpleError(paste0("Unknown tariff: ", tariff)))
  )
  if(year < 2014 | year > 2021){
    minPrice <- minPriceSwitch[length(minPriceSwitch)]
  } else {
    minPrice <- minPriceSwitch[year - 2013]
  }
  return(minPrice)
} ## end function min_price()


## function globalDiscount()
# returns a named vector including the effective factor, reduction and net result
# net: numeric, net sum
# discount: numeric between 0 and 1, multiplied with net
# factor: whether a discount should be calculated in the first place
globalDiscount <- function(net, discount, factor=TRUE){
  result <- c()
  if(isTRUE(factor) & discount < 1){
    stopifnot(discount >= 0)
    result["factor"] <- discount
    result["discount"] <- (net * discount) - net
    result["tariff"] <- net * discount
  } else {
    result["factor"] <- 1
    result["discount"] <- 0
    result["tariff"] <- net
  }
  return(result)
} ## end function globalDiscount()


## function invoice()
# returns a matrix
# mqy: if TRUE calculates prices vor monthly, quarterly and yearly periods
invoice <- function(tariff, taxFactor=1.07, GVLFactor=1.26, mqy=TRUE){
  result <- sapply(
    c("m","q","y"),
    function(period){
      reductionPeriod <- reduction_period(period=period)
      periodFactor <- factor_period(period=period)

      GEMA <- tariff * periodFactor * reductionPeriod
      GVL <- GEMA * (GVLFactor - 1)
      net  <- GEMA * GVLFactor
      VAT <- net * (taxFactor - 1)
      gross <- net * taxFactor
      return(c(GEMA, GVL, net, VAT, gross))
    },
    USE.NAMES=FALSE
  )
  if(isTRUE(mqy)){
    dimnames(result) <- list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year"))
  } else {
    result <- as.matrix(result[,1])
    dimnames(result) <- list(c("GEMA", "GVL","net", "VAT", "gross"),c("tariff"))
  }
  return(result)
} ## end function invoice()
