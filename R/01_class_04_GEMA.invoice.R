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

#' S4 Class GEMA.invoice
#'
#' This class is inherited by other invoice classes and not standalone.
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
#' @slot tariff A named list containing the actual monthly tariff net sums without reductions:
#'     \describe{
#'        \item{GEMA}{GEMA net tariff.}
#'        \item{GVL}{GVL net tariff.}
#'      }
#' @slot invoice A named list containing the final invoice sums:
#'     \describe{
#'        \item{GEMA}{Final GEMA net tariff.}
#'        \item{GVL}{Final GVL net tariff.}
#'        \item{net}{The net sum.}
#'        \item{VAT}{The applicable VAT tax amount.}
#'        \item{gross}{The gross sum.}
#'      }
#' @name GEMA.invoice,-class
#' @aliases GEMA.invoice,-class GEMA.invoice-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.invoice-class
setClass("GEMA.invoice",
  representation=representation(
    factors="numeric",
    categories="numeric",
    tariff="matrix",
    invoice="matrix"
  ),
  prototype(
    factors=c(
      GVL=1,
      VAT=1,
      launch=1
    ),
    categories=c(
      space=1,
      admission=1
    ), 
    tariff=matrix(
      rep(0,6),
      ncol=3,
      dimnames=list(c("GEMA", "GVL"),c("month","quarter","year"))
    ),
    invoice=matrix(
      rep(0,15),
      ncol=3,
      dimnames=list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year"))
    )
  )
)
