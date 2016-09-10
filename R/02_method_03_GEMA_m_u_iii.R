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

#' Calculate GEMA tariff M U-III
#' 
#' @param obj An object of class \code{\link[GEMATariff]{GEMA.dancefloor-class}}.
#' @param discount Logical, whether a reduction due to a global contract ("Gesamtvertrag") is eligable. As of now, a reduction of 20\%
#'    is granted if this is set to \code{TRUE}.
#' @export
#' @docType methods
#' @rdname tariff-methods
setGeneric("GEMA_m_u_iii", function(dancefloor, discount=FALSE){standardGeneric("GEMA_m_u_iii")})

#' @export
#' @docType methods
#' @rdname tariff-methods
#' @aliases GEMA_m_u_iii GEMA_m_u_iii,GEMA.dancefloor-method
#' @include 01_class_01_GEMA.dancefloor.R
setMethod("GEMA_m_u_iii",
  signature(dancefloor="GEMA.dancefloor"),
  function (dancefloor, discount=FALSE){
  
    results <- new("GEMA.invoice_m_cd", dancefloor=dancefloor)
    result.factors <- slot(results, "factors")       # c(GVL=1.26, VAT=1.07, launch=1)
    result.categories <- slot(results, "categories") # c(space=1, admission=1)
    result.tariff  <- slot(results, "tariff")        # matrix(rep(0,6), ncol=3, dimnames=list(c("GEMA", "GVL"),c("month","quarter","year")))
    result.invoice  <- slot(results, "invoice")      # matrix(rep(0,15), ncol=3, dimnames=list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year")))
    result.reduction <- c()

    days <- 4 * slot(dancefloor, "days")
    space <- slot(dancefloor, "space")
    admission <- slot(dancefloor, "admission")
    
    GVLFactor <- 1.26
    result.factors["GVL"] <- GVLFactor

    result.factors["VAT"] <- 1.07

    # the matrix tariff_matrix_m_u_iii_1_c_2012 is defined in GEMATariff-internal_tariff_tables.R
    if(days > 16){
      first100 <- tariff_matrix_m_u_iii_1_c_2012["first100","gt16"]
      each100 <- tariff_matrix_m_u_iii_1_c_2012["each100","gt16"]
    } else {
      first100 <- tariff_matrix_m_u_iii_1_c_2012["first100","le16"]
      each100 <- tariff_matrix_m_u_iii_1_c_2012["each100","le16"]
    }
    
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
