# Copyright 2016-2017 Meik Michalke <meik.michalke@c3s.cc>
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

#' Calculate GEMA tariff U-K I (Wiedergaben mit Musikern)
#' 
#' @param obj An object of class \code{\link[GEMATariff]{GEMA.gig-class}}.
#' @param byGross Locical, whether calculations should be based on \code{gross} value instead of code{guests} \times{} \code{admission}.
#' @param year Integer. If between 2015 and 2022, the market entry reduction will be calculated.
#'    Set to a different year to disable.
#' @param discount Logical, whether a reduction due to a global contract ("Gesamtvertrag") is eligable. As of now, a reduction of 20\%
#'    is granted if this is set to \code{TRUE}.
#' @references
#'    \url{https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_u_k.pdf}
#' @export
#' @docType methods
#' @rdname tariff-methods
setGeneric("GEMA_u_k", function(gig, byGross=TRUE, year=as.numeric(format(Sys.time(), "%Y")), discount=FALSE){standardGeneric("GEMA_u_k")})

#' @export
#' @docType methods
#' @rdname tariff-methods
#' @aliases GEMA_u_k GEMA_u_k,GEMA.gig-method
#' @include 01_class_03_GEMA.gig.R
setMethod("GEMA_u_k",
  signature(gig="GEMA.gig"),
  function(gig, byGross=TRUE, year=as.numeric(format(Sys.time(), "%Y")), discount=FALSE){
  
    results <- new("GEMA.invoice_u_k", gig=gig)
    result.factors <- slot(results, "factors")       # c(GVL=1.26, VAT=1.07, launch=1)
    result.categories <- slot(results, "categories") # c(space=1, admission=1)
    result.tariff  <- slot(results, "tariff")        # matrix(rep(0,6), ncol=3, dimnames=list(c("GEMA", "GVL"),c("month","quarter","year")))
    result.invoice  <- slot(results, "invoice")      # matrix(rep(0,15), ncol=3, dimnames=list(c("GEMA", "GVL","net", "VAT", "gross"),c("month","quarter","year")))
    result.reduction <- slot(results, "reduction")

    gross <- slot(gig, "gross")
    guests <- slot(gig, "guests")
    admission <- slot(gig, "admission")
    musicians <- slot(gig, "musicians")
    addIncome <- slot(gig, "addIncome")
    public <- slot(gig, "public")
    fee <- slot(gig, "fee")
    production <- slot(gig, "production")
    charity <- slot(gig, "charity")
    rcs <- slot(gig, "rcs")
    promoYoung <- slot(gig, "promoYoung")
    lumpSumContract <- slot(gig, "lumpSumContract")
    plusMedium <- slot(gig, "plusMedium")

    if(!isTRUE(musicians)){
      GVLFactor <- 1.2
    } else if(isTRUE(plusMedium)){
      GVLFactor <- 1.1
    } else {
      GVLFactor <- 1
    }
    result.factors["GVL"] <- GVLFactor

    result.factors["VAT"] <- 1.07

    if(year > 2014){
      # check applicability of reductions
      if(isTRUE(public) & !isTRUE(admission == 0)){
        warning("Admission is not 0€, 'public' will be treated as FALSE!", call.=FALSE)
        public <- FALSE
      } else {}
      if(isTRUE(promoYoung)){
        if(admission > 20){
          warning("Admission is >20€, 'promoYoung' will be treated as FALSE!", call.=FALSE)
          promoYoung <- FALSE
        } else {}
        if(guests > 300){
          warning("More than 300 guests, 'promoYoung' will be treated as FALSE!", call.=FALSE)
          promoYoung <- FALSE
        } else {}
      } else {}
    } else {
      public <- promoYoung <- rcs <- FALSE
    }
    # base sum is not the actual tariff, but the amount of money the percentage is to be taken from
    if(isTRUE(public)){
      baseSum <- fee + production
    } else {
      if(isTRUE(byGross)){
        baseSum <- gross
      } else {
        baseSum <- guests * admission
      }
    }
    
    # entry reductions
    # these might be replaced effectively by smaller rates if promoYoung is TRUE
    if(year > 2015){
      if(guests > 2000){
        entryRedPercent <- reduction_entry(year=year, tariff="u_k_gt2000")
      } else {
        entryRedPercent <- reduction_entry(year=year, tariff="u_k_le2000")
      }
      entryAddIncome <- reduction_entry(year=year, tariff="u_k_addIncome")
    } else if(year == 2015){
      # this year needs more special treatment...
      if(guests > 15000){
        entryRedPercent <- .0765
        entryAddIncome <- .0042
      } else if(guests > 2000){
        entryRedPercent <- reduction_entry(year=year, tariff="u_k_gt2000")
        entryAddIncome <- .0038
      } else {
        entryRedPercent <- reduction_entry(year=year, tariff="u_k_le2000")
        entryAddIncome <- reduction_entry(year=year, tariff="u_k_addIncome")
      }
    } else if(year > 2012){
      if(year == 2013){
        entryRedPercent <- category_steps(value=guests, steps=c(2000,15000), names=c(.045,.052,.0685))
      } else {
        entryRedPercent <- category_steps(value=guests, steps=c(2000,15000), names=c(.05,.072,.0765))
      }
      entryAddIncome <- category_steps(value=guests, steps=c(2000,15000), names=c(.0035,.0038,.0042))
    } else {
      stop(simpleError("This tariff can't be calculated for years before 2013!"))
    }
    result.factors["launch"] <- entryRedPercent

    if(year > 2014){
      guestsClass <-  category_steps(value=guests, steps=150, div=TRUE)
      result.categories["guests"] <- guestsClass
      each150 <- min_price(year=year, tariff="u_k_each150")
      minPrice <- each150 * guestsClass
    } else {
      if(guests > 15000){
        guestsFactor <- category_steps(value=guests - 15000, steps=1500, div=TRUE)
        result.categories["guests"] <- 16 + guestsFactor
        min15000 <- tariff_matrix_u_k_2013["le15000","min"]
        each1500 <- tariff_matrix_u_k_2013["each1500","min"]
        minPrice <- min15000 + (each1500 * guestsFactor)
      } else {
        guestsClass <- category_steps(value=guests,
          steps=c(150,300,600,1200,1800,2400,3000,4500,6000,7500,9000,10500,12000,13500,15000)
        )
        result.categories["guests"] <- guestsClass
        minPrice <- tariff_matrix_u_k_2013[guestsClass,"min"]
      }
    }

    if(isTRUE(addIncome)){
      result.factors["addIncome"] <- 1 + entryAddIncome
      basePercent <- entryRedPercent + entryAddIncome
    } else {
      result.factors["addIncome"] <- 1
      basePercent <- entryRedPercent
    }
    basePrice <- max(baseSum * basePercent, minPrice)

    # check reductions
    if(isTRUE(promoYoung)){
      # reduction is relative to baseSum directly, not basePrice
      basePricePromoYoung <- max(
        baseSum * result.factors["addIncome"] * reduction_entry(year, "u_k_promoYoung"),
        minPrice
      )
      result.factors["promoYoung"] <- 1 - (basePricePromoYoung / basePrice)
      result.reduction["promoYoung"] <- basePricePromoYoung - basePrice
    } else {
      result.factors["promoYoung"] <- 1
      result.reduction["promoYoung"] <- 0
    }

    discountCharity <- globalDiscount(net=basePrice, discount=.9, factor=charity)
    result.factors["charity"] <- discountCharity[["factor"]]
    result.reduction["charity"] <- discountCharity[["discount"]]

    discountRcs <- globalDiscount(net=basePrice, discount=.85, factor=rcs)
    result.factors["rcs"] <- discountRcs[["factor"]]
    result.reduction["rcs"] <- discountRcs[["discount"]]

    if(lumpSumContract == 16){
      result.factors["lumpSumContract"] <- .9
      result.reduction["lumpSumContract"] <- (basePrice * .9) - basePrice
    } else if(lumpSumContract == 31){
      result.factors["lumpSumContract"] <- .855
      result.reduction["lumpSumContract"] <- (basePrice * .855) - basePrice
    } else {
      result.reduction["lumpSumContract"] <- 0
      result.factors["lumpSumContract"] <- 1
    }
    # combine all reductions
    basePrice <- basePrice + sum(result.reduction[c("promoYoung","charity","rcs","lumpSumContract")])

    # global discount
    result.factors["discount"] <-  ifelse(isTRUE(discount), .8, 1)
    discountTariff <- globalDiscount(net=basePrice, discount=result.factors["discount"])
    result.reduction["discount"] <- discountTariff[["discount"]]
    tariff <- discountTariff[["tariff"]]

    result.invoice <- invoice(
      tariff=tariff,
      taxFactor=result.factors["VAT"],
      GVLFactor=result.factors["GVL"],
      mqy=FALSE
    )

    # how much did the entry discount save?
    if(year > 2014 & entryRedPercent < 0.1){
      # calculate tariff without entry discount
      tariffOrig <- GEMA_u_k(gig=gig, year=2021, discount=discount)
      result.tariff <- slot(tariffOrig, "tariff")
      result.reduction["launch"] <- result.invoice["GEMA",] - result.tariff["GEMA",]
    } else {
      result.tariff <- result.invoice
      result.reduction["launch"] <- 0
    }
    # compare new tariffs
    if(year > 2014){
      tariff2014 <- slot(GEMA_u_k(gig=gig, year=2014, discount=discount), "invoice")
      result.reduction["tariff2014"] <- tariff2014["GEMA",]
      result.reduction["tariffdiff"] <- result.invoice["GEMA",] - result.reduction["tariff2014"]
    } else {}

    slot(results, "factors") <- result.factors
    slot(results, "categories") <- result.categories
    slot(results, "tariff") <- result.tariff
    slot(results, "invoice") <- result.invoice
    slot(results, "reduction") <- result.reduction

    return(results)
  }
)
