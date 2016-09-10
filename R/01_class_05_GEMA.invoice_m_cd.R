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

#' S4 Class GEMA.invoice_m_cd
#'
#' This class is used for objects that are returned by methods like \code{\link[GEMATariff:GEMA_m_cd]{GEMA_m_cd}}.
#'
#' @slot factors A named list of factors that affect the end result:
#'    \describe{
#'      \item{GVL}{Fees for GVL.}
#'      \item{VAT}{Value added tax.}
#'      \item{launch}{Reduction factor for market introduction of the new tariffs.}
#'    }
#' @slot categories A named list of price categories:
#'    \describe{
#'      \item{space}{Space category (in 100 square meter steps).}
#'      \item{admission}{Admission category (in 2€ steps).}
#'    }
#' @slot dancefloor An object of class \code{\link[GEMATariff]{GEMA.dancefloor-class}}.
#' @slot tariff A named list containing the actual monthly tariff net sums without reductions:
#'     \describe{
#'        \item{GEMA}{GEMA net tariff.}
#'        \item{GVL}{GVL net tariff.}
#'      }
#' @slot reduction A named list of applicable reductions for the montly GEMA net tariff:
#'     \describe{
#'        \item{tariff2012}{The original old tariff net sum (as of 2012), to calculate the launch reduction.}
#'        \item{tariffdiff}{Difference between the old tariff (plus 6.5\%) and the new one.}
#'        \item{launch}{The amount of reduction applicable (applying \code{launch}).}
#'        \item{discount}{Other global discount ("Gesamtvertragsnachlass").}
#'      }
#' @slot invoice A named list containing the final invoice sums:
#'     \describe{
#'        \item{GEMA}{Final GEMA net tariff.}
#'        \item{GVL}{Final GVL net tariff.}
#'        \item{net}{The net sum.}
#'        \item{VAT}{The applicable VAT tax amount.}
#'        \item{gross}{The gross sum.}
#'      }
#' @name GEMA.invoice_m_cd,-class
#' @aliases GEMA.invoice_m_cd,-class GEMA.invoice_m_cd-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.invoice_m_cd-class
#' @include 01_class_01_GEMA.dancefloor.R
#' @include dancefloor.R
setClass("GEMA.invoice_m_cd",
  representation=representation(
    dancefloor="GEMA.dancefloor",
    reduction="numeric"
  ),
  prototype(
    dancefloor=dancefloor(),
    reduction=c(
      tariff2012=0,
      tariffdiff=0,
      launch=0,
      discount=0
    )
  ),
  contains="GEMA.invoice"
)
