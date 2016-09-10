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

#' Calculate GEMA tariff U-T I (Musikaufführungen in Tanzlokalen)
#' 
#' @param obj An object of class \code{\link[GEMATariff]{GEMA.liveclub-class}}.
#' @param discount Logical, whether a reduction due to a global contract ("Gesamtvertrag") is eligable. As of now, a reduction of 20\%
#'    is granted if this is set to \code{TRUE}.
#' @references
#'    \url{https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_u_t.pdf}
#' @export
#' @docType methods
#' @rdname tariff-methods
setGeneric("GEMA_u_t", function(liveclub, discount=FALSE){standardGeneric("GEMA_u_t")})

#' @export
#' @docType methods
#' @rdname tariff-methods
#' @aliases GEMA_u_t GEMA_u_t,GEMA.liveclub-method
#' @include 01_class_02_GEMA.liveclub.R
setMethod("GEMA_u_t",
  signature(liveclub="GEMA.liveclub"),
  function(liveclub, discount=FALSE){
  
    results <- new("GEMA.invoice_u_t", liveclub=liveclub)
    result.factors <- slot(results, "factors")[c("GVL","VAT")] # c(GVL=1.26, VAT=1.07)
    result.categories <- slot(results, "categories") # c(space=1, admission=1)
    result.tariff  <- slot(results, "tariff")        # matrix(rep(0,6), ncol=3, dimnames=list(c("GEMA", "GVL"),c("month","quarter","year")))
    result.invoice  <- slot(results, "invoice")      # matrix(rep(0,15), ncol=3, dimnames=list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year")))
    result.reduction <- slot(results, "reduction")

    days <- slot(liveclub, "days")
    space <- slot(liveclub, "space")
    admission <- slot(liveclub, "admission")
    plusMedium <- slot(liveclub, "plusMedium")

    GVLFactor <- ifelse(isTRUE(plusMedium), 1.10, 1)
    result.factors["GVL"] <- GVLFactor

    result.factors["VAT"] <- 1.07

    # the list tariff_matrix_u_t is defined in GEMATariff-internal_tariff_tables.R
    if(days > 16){
      tariff_matrix <- tariff_matrix_u_t[["gt16"]]
    } else {
      tariff_matrix <- tariff_matrix_u_t[["le16"]]
    }

    priceClass <- category_steps(admission, c(2.5, 5, 10))
    result.categories["admission"] <- priceClass
    priceClassName <-  category_steps(admission, c(2.5, 5, 10), names=c("le250","le500","le1000","gt1000"))

    first100 <- tariff_matrix["first100",priceClassName]
    each100 <- tariff_matrix["each100",priceClassName]

    spaceCategory <- category_space(space=space)
    result.categories["space"] <- spaceCategory

    if(spaceCategory > 1){
      tariff <- first100 + ((spaceCategory - 1) * each100)
    } else {
      tariff <- first100
    }

    # global discount
    result.factors["discount"] <-  ifelse(isTRUE(discount), .8, 1)
    dicountTariff <- globalDiscount(net=tariff, discount=result.factors["discount"])
    result.reduction["discount"] <- dicountTariff[["discount"]]
    tariff <- dicountTariff[["tariff"]]

    result.invoice <- invoice(
      tariff=tariff,
      taxFactor=result.factors["VAT"],
      GVLFactor=result.factors["GVL"]
    )
    result.tariff["GEMA",] <- result.invoice["GEMA",]
    result.tariff["GVL",] <- result.invoice["GVL",]

    slot(results, "factors") <- result.factors
    slot(results, "categories") <- result.categories
    slot(results, "tariff") <- result.tariff
    slot(results, "reduction") <- result.reduction
    slot(results, "invoice") <- result.invoice

    return(results)
  }
)
