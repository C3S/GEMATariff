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

#' S4 Class GEMA.invoice_u_t
#'
#' This class is used for objects that are returned by methods like \code{\link[GEMATariff:GEMA_u_t]{GEMA_u_t}}.
#'
#' @slot factors A named list of factors that affect the end result:
#'    \describe{
#'      \item{GVL}{Fees for GVL.}
#'      \item{VAT}{Value added tax.}
#'    }
#' @slot categories A named list of price categories:
#'    \describe{
#'      \item{space}{Space category (in 100 square meter steps).}
#'      \item{admission}{Admission category (in 2€ steps).}
#'    }
#' @slot liveclub An object of class \code{\link[GEMATariff]{GEMA.liveclub-class}}.
#' @slot tariff A named list containing the actual monthly tariff net sums without reductions:
#'     \describe{
#'        \item{GEMA}{GEMA net tariff.}
#'        \item{GVL}{GVL net tariff.}
#'      }
#' @slot reduction A named list of applicable reductions for the GEMA net tariff:
#'     \describe{
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
#' @name GEMA.invoice_u_t,-class
#' @aliases GEMA.invoice_u_t,-class GEMA.invoice_u_t-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.invoice_u_t-class
#' @include 01_class_02_GEMA.liveclub.R
#' @include liveclub.R
setClass("GEMA.invoice_u_t",
  representation=representation(
    liveclub="GEMA.liveclub",
    reduction="numeric"
  ),
  prototype(
    liveclub=liveclub(),
    reduction=c(
      discount=0
    )
  ),
  contains="GEMA.invoice"
)
