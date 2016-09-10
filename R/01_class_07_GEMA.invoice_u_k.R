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

#' S4 Class GEMA.invoice_u_k
#'
#' This class is used for objects that are returned by methods like \code{\link[GEMATariff:GEMA_u_k]{GEMA_u_k}}.
#'
#' @slot factors A named list of factors that affect the end result:
#'    \describe{
#'      \item{GVL}{Fees for GVL.}
#'      \item{VAT}{Value added tax.}
#'      \item{launch}{Reduction factor for market introduction of the new tariffs.}
#'    }
#' @slot categories A named list of price categories:
#'    \describe{
#'      \item{guests}{Guests category (in 150 guests steps).}
#'    }
#' @slot gig An object of class \code{\link[GEMATariff]{GEMA.gig-class}}.
#' @slot tariff A named list containing the actual monthly tariff net sums without reductions:
#'     \describe{
#'        \item{GEMA}{GEMA net tariff.}
#'        \item{GVL}{GVL net tariff.}
#'      }
#' @slot reduction A named list of applicable reductions for the montly GEMA net tariff:
#'     \describe{
#'        \item{charity}{The amount of reduction applicable for charity.}
#'        \item{rcs}{The amount of reduction applicable for religious, cultural or social character.}
#'        \item{promoYoung}{The amount of reduction applicable for promoting young musicians.}
#'        \item{yearly}{The amount of reduction applicable for a yearly contrat in place.}
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
#' @name GEMA.invoice_u_k,-class
#' @aliases GEMA.invoice_u_k,-class GEMA.invoice_u_k-class
#' @import methods
#' @keywords classes
#' @export
#' @rdname GEMA.invoice_u_k-class
#' @include 01_class_03_GEMA.gig.R
#' @include gig.R
setClass("GEMA.invoice_u_k",
  representation=representation(
    gig="GEMA.gig",
    reduction="numeric"
  ),
  prototype(
    gig=gig(),
    reduction=c(
      charity=0,
      rcs=0,
      promoYoung=0,
      yearly=0,
      launch=0,
      discount=0
    )
  ),
  contains="GEMA.invoice"
)
