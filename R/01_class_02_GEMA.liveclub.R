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

#' S4 Class GEMA.liveclub
#'
#' This class is used for objects that define one live club.
#' 
#' Use function \code{\link[GEMATariff:liveclub]{liveclub}} to create objects of this class.
#'
#' @slot space Numeric, room size for the live club in square meters.
#' @slot days Integer number between 1 and 30, number of days the club is opened per month.
#' @slot admission Numeric, highest ticket price for the live club. If prices vary over the week,
#'    use the average of highest prices, respectively.
#' @slot plusMedium Logical, whether in addition to the live music there is also music from
#'    physical recording media (e.g., CDs) being played.
#' @name GEMA.liveclub,-class
#' @aliases GEMA.liveclub,-class GEMA.liveclub-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.liveclub-class
#' @include 01_class_01_GEMA.dancefloor.R
#' @examples
#' myLiveClub <- liveclub(space=200, days=3, admission=6)
setClass("GEMA.liveclub",
  representation=representation(
    space="numeric",
    days="numeric",
    admission="numeric",
    plusMedium="logical"
  ),
  prototype(
    space=numeric(),
    days=numeric(),
    admission=numeric(),
    plusMedium=FALSE
  )
)

setValidity("GEMA.liveclub",
  function(object){
    days <- slot(object, "days")
    if(days < 1 | days > 30){
      stop(simpleError("'days' must be between 1 and 30!"))
    }
    return(TRUE)
  }
)
