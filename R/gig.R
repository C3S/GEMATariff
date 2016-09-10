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

#' Define a live concert
#' 
#' There are two different modes to calculate the tariff for concerts. If you provide the exact
#' gross revenue rained through the concert, that amount is used as the calculation basis. If not,
#' the product of ticket price and number of guests will be used as an estimate. 
#' 
#' @param gross Numeric, the total gross receipts for the concert.
#' @param guests Numeric, number of people attending the concert.
#' @param admission Numeric, highest ticket price for the concert.
#' @param musicians Logical, whether the music ist mostly perfomed by live musicians (in contrast to
#'    mostly by using physical recording media).
#' @param addIncome Logical, whether you have additional income through advertising etc.
#' @param fee Numeric, the amount of money payed to the artists, relevant if this is a concert
#'    for a strictly limited autience or public without any admission fee.
#' @param production Numeric, the directly associated production costs, relevant if \code{fee} is relevant, too.
#' @param charity Logical, whether the event is of non-profit/charitable character.
#' @param rcs Logical, whether the event is of religious, cultural, or social character.
#' @param promoYoung Logical, whether this event is only promoting young musicians.
#' @param lumpSumContract Numeric, one of the following to declare it you have a yearly contract:
#'    \describe{
#'      \item{0}{No yearly contract or not more than 15 events a year.}
#'      \item{16}{Yearly contract and at least 16 events a year.}
#'      \item{31}{Yearly contract and at least 31 events a year.}
#'    }
#' @param plusMedium Logical, whether in addition to the live music there is also music from
#'    physical recording media (e.g., CDs) being played.
#' @return An object of class \code{\link[GEMATariff]{GEMA.gig-class}}.
#' @export
#' @include 01_class_03_GEMA.gig.R
#' @examples
#' myGig <- gig(guests=100, admission=5)
gig <- function(
    gross=0,
    guests=0,
    admission=0,
    musicians=TRUE,
    addIncome=FALSE,
    fee=0,
    production=0,
    charity=FALSE,
    rcs=FALSE,
    promoYoung=FALSE,
    lumpSumContract=0,
    plusMedium=FALSE
  ){
  return(
    new("GEMA.gig",
      gross=gross,
      guests=guests,
      admission=admission,
      musicians=musicians,
      addIncome=addIncome,
      fee=fee,
      production=production,
      charity=charity,
      rcs=rcs,
      promoYoung=promoYoung,
      lumpSumContract=lumpSumContract,
      plusMedium=plusMedium
    )
  )
}
