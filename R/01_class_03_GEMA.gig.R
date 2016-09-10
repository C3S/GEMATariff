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

#' S4 Class GEMA.gig
#'
#' This class is used for objects that define a live concert.
#' 
#' Use function \code{\link[GEMATariff:gig]{gig}} to create objects of this class.
#'
#' @slot gross Numeric, the total gross receipts for the concert.
#' @slot guests Numeric, number of people attending the concert.
#' @slot admission Numeric, highest ticket price for the concert.
#' @slot musicians Logical, whether the music ist mostly perfomed by live musicians (in contrast to
#'    mostly by using physical recording media).
#' @slot addIncome Logical, whether you have additional income through advertising etc.
#' @slot public Logical, whether the concert was either public with no admission fee, or an invite-only
#'    event for a strictly limited amount of guests (e.g., a promotional event). If \code{TRUE}, \code{fee}
#'    and \code{production} become relevant, and \code{admission} must be 0.
#' @slot fee Numeric, the amount of money payed to the artists, relevant if \code{public=TRUE}.
#' @slot production Numeric, the directly associated production costs, relevant if \code{public=TRUE}.
#' @slot charity Logical, whether the event is of non-profit/charitable character.
#' @slot rcs Logical, whether the event is of religious, cultural, or social character.
#' @slot promoYoung Logical, whether this event is only promoting young musicians.
#' @slot lumpSumContract Numeric, one of the following to declare it you have a yearly contract:
#'    \describe{
#'      \item{0}{No yearly contract or not more than 15 events a year.}
#'      \item{16}{Yearly contract and at least 16 events a year.}
#'      \item{31}{Yearly contract and at least 31 events a year.}
#'    }
#' @slot plusMedium Logical, whether in addition to the live music there is also music from
#'    physical recording media (e.g., CDs) being played.
#' @name GEMA.gig,-class
#' @aliases GEMA.gig,-class GEMA.gig-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.gig-class
#' @examples
#' myGig <- gig()
setClass("GEMA.gig",
  representation=representation(
    gross="numeric",
    guests="numeric",
    admission="numeric",
    musicians="logical",
    addIncome="logical",
    public="logical",
    fee="numeric",
    production="numeric",
    charity="logical",
    rcs="logical",
    promoYoung="logical",
    lumpSumContract="numeric",
    plusMedium="logical"
  ),
  prototype(
    gross=numeric(),
    guests=numeric(),
    admission=numeric(),
    musicians=TRUE,
    addIncome=FALSE,
    public=FALSE,
    fee=numeric(),
    production=numeric(),
    charity=FALSE,
    rcs=FALSE,
    promoYoung=FALSE,
    lumpSumContract=0,
    plusMedium=FALSE
  )
)

setValidity("GEMA.gig",
  function(object){
    lumpSumContract <- slot(object, "lumpSumContract")
    if(!lumpSumContract %in% c(0,16,31)){
      stop(simpleError("'lumpSumContract' must be one of 0, 16, or 31!"))
    }
    return(TRUE)
  }
)
