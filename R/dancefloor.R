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

#' Define a dancefloor
#' 
#' @param space Numeric, room size for the dancefloor in square meters.
#' @param days Integer number between 1 and 7, number of days the club is opened per week.
#' @param admission Numeric, highest ticket price for the dancefloor. If prices vary over the week, use the average of highest prices, respectively.
#' @return An object of class \code{\link[GEMATariff]{GEMA.dancefloor-class}}.
#' @export
#' @include 01_class_01_GEMA.dancefloor.R
#' @examples
#' myDancefloor <- dancefloor(space=200, days=3, admission=6)
dancefloor <- function(space=100, days=1, admission=0){
  return(
    new("GEMA.dancefloor",
      space=space,
      days=days,
      admission=admission
    )
  )
}