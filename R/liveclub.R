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

#' Define a live club
#' 
#' @param space Numeric, room size for the live club in square meters.
#' @param days Integer number between 1 and 30, number of days the club is opened per month.
#' @param admission Numeric, highest ticket price for the live club. If prices vary over the week,
#'    use the average of highest prices, respectively.
#' @param plusMedium Logical, whether in addition to the live music there is also music from
#'    physical recording media (e.g., CDs) being played.
#' @return An object of class \code{\link[GEMATariff]{GEMA.liveclub-class}}.
#' @export
#' @include 01_class_02_GEMA.liveclub.R
#' @examples
#' myLiveClub <- liveclub(space=200, days=20, admission=6, plusMedium=TRUE)
liveclub <- function(space=100, days=1, admission=0, plusMedium=FALSE){
  return(
    new("GEMA.liveclub",
      space=space,
      days=days,
      admission=admission,
      plusMedium=plusMedium
    )
  )
}