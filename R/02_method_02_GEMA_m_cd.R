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

#' S4 methods to calculate tariffs for given dancefloors
#' 
#' The method \code{GEMA_m_u_iii} calculates GEMA tarifs as of 2012, \code{GEMA_m_cd} calculates
#' the new tariffs as of 2014.
#' 
#' @param obj An object of class \code{\link[GEMATariff]{GEMA.dancefloor-class}}.
#' @param year Integer. If between 2014 and 2021, the market entry reduction will be calculated.
#'    Set to a different year to disable.
#' @param discount Logical, whether a reduction due to a global contract ("Gesamtvertrag") is eligable. As of now, a reduction of 20\%
#'    is granted if this is set to \code{TRUE}.
#' @references
#'    \url{https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_AD/tarif_m_cd.pdf}
#' @export
#' @docType methods
#' @rdname tariff-methods
setGeneric("GEMA_m_cd", function(dancefloor, year=as.numeric(format(Sys.time(), "%Y")), discount=FALSE){standardGeneric("GEMA_m_cd")})

#' @export
#' @docType methods
#' @rdname tariff-methods
#' @aliases GEMA_m_cd GEMA_m_cd,GEMA.dancefloor-method
#' @include 01_class_01_GEMA.dancefloor.R
setMethod("GEMA_m_cd",
  signature(dancefloor="GEMA.dancefloor"),
  function (dancefloor, year=as.numeric(format(Sys.time(), "%Y")), discount=FALSE){
    results <- new("GEMA.invoice_m_cd", dancefloor=dancefloor)
    result.factors <- slot(results, "factors")       # c(GVL=1.26, VAT=1.07, launch=1)
    result.categories <- slot(results, "categories") # c(space=1, admission=1)
    result.tariff  <- slot(results, "tariff")        # matrix(rep(0,6), ncol=3, dimnames=list(c("GEMA", "GVL"),c("month","quarter","year")))
    result.reduction <- slot(results, "reduction")   # c(tariff2012=0, tariffdiff=0, launch=0)
    result.invoice  <- slot(results, "invoice")      # matrix(rep(0,15), ncol=3, dimnames=list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year")))

    days <- slot(dancefloor, "days")
    space <- slot(dancefloor, "space")
    admission <- slot(dancefloor, "admission")
    
    GVLFactor <- 1.26
    result.factors["GVL"] <- GVLFactor

    result.factors["VAT"] <- 1.07

    if(days < 4){
      min <- tariff_matrix_m_cd["min",days]
      each2 <- tariff_matrix_m_cd["each2",days]
    } else {
      min <- days * tariff_matrix_m_cd["min",4]
      each2 <- days * tariff_matrix_m_cd["each2",4]
    }
    priceClass <- category_steps(admission, steps=2, div=TRUE)
    result.categories["admission"] <- priceClass

    if(priceClass > 1){
      baseprice <- min + ((priceClass - 1) * each2)
    } else {
      baseprice <- min
    }
    roomFactor <- category_space(space=space)
    result.categories["space"] <- roomFactor

    tariff <- baseprice * roomFactor
    invoice.full <- invoice(
      tariff=tariff,
      taxFactor=result.factors["VAT"],
      GVLFactor=result.factors["GVL"]
    )
    result.tariff["GEMA",] <- invoice.full["GEMA",]
    result.tariff["GVL",] <- invoice.full["GVL",]

    # reduction of new net sum os higher than the old
    tariffOld <- slot(GEMA_m_u_iii(dancefloor=dancefloor, discount=discount), "invoice")
    result.reduction["tariff2012"] <- tariffOld["GEMA","month"]
    result.reduction["tariffdiff"] <- tariff - (result.reduction["tariff2012"] * 1.065)
    if(result.reduction["tariffdiff"] > 0){
      reductionEntry <- reduction_entry(year=year, tariff="m_cd")
      result.factors["launch"] <- reductionEntry
      result.reduction["launch"] <- result.reduction["tariffdiff"] * reductionEntry * -1
      tariff <- tariff + result.reduction["launch"]
    }

    # global discount
    result.factors["discount"] <- ifelse(isTRUE(discount), .8, 1)
    dicountTariff <- globalDiscount(net=tariff, discount=result.factors["discount"])
    result.reduction["discount"] <- dicountTariff[["discount"]]
    tariff <- dicountTariff[["tariff"]]

    result.invoice <- invoice(
      tariff=tariff,
      taxFactor=result.factors["VAT"],
      GVLFactor=result.factors["GVL"]
    )

    slot(results, "factors") <- result.factors
    slot(results, "categories") <- result.categories
    slot(results, "tariff") <- result.tariff
    slot(results, "reduction") <- result.reduction
    slot(results, "invoice") <- result.invoice

    return(results)
  }
)
