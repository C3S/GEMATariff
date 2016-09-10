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

#' S4 Class GEMA.dancefloor
#'
#' This class is used for objects that define one dancefloor.
#' 
#' Use function \code{\link[GEMATariff:dancefloor]{dancefloor}} to create objects of this class.
#'
#' @slot space Numeric, room size for the dancefloor in square meters.
#' @slot days Integer number between 1 and 7, number of days the club is opened per week.
#' @slot admission Numeric, highest ticket price for the dancefloor. If prices vary over the week,
#'    use the average of highest prices, respectively.
#' @name GEMA.dancefloor,-class
#' @aliases GEMA.dancefloor,-class GEMA.dancefloor-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.dancefloor-class
#' @examples
#' myDancefloor <- dancefloor(space=200, days=3, admission=6)
setClass("GEMA.dancefloor",
  representation=representation(
    space="numeric",
    days="numeric",
    admission="numeric"
  ),
  prototype(
    space=numeric(),
    days=numeric(),
    admission=numeric()
  )
)

setValidity("GEMA.dancefloor",
  function(object){
    days <- slot(object, "days")
    if(days < 1 | days > 7){
      stop(simpleError("'days' must be between 1 and 7!"))
    }
    return(TRUE)
  }
)
