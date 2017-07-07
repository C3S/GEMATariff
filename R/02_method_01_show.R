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

#' Show methods for GEMATariff objects
#' 
#' @param object An object of class
#'    \code{GEMA.dancefloor},
#'    \code{GEMA.liveclub},
#'    \code{GEMA.gig},
#'    \code{GEMA.invoice_u_t},
#'     or
#'    \code{GEMA.invoice_m_cd}.
#' @export
#' @docType methods
#' @aliases show,-methods
#' @rdname show-methods
#' @examples
#' \dontrun{
#' MTLD(tagged.txt)
#' }
#' @include 01_class_01_GEMA.dancefloor.R
#' @aliases show,GEMA.dancefloor-method
setMethod(
  "show",
  signature(object="GEMA.dancefloor"),
  function(object){
    days <- slot(object, "days")
    dayName <- ifelse(days > 1, "days", "day")
    cat(paste0(
      "\n   Dancefloor\n\n",
      "        Doors open: ", days, " ", dayName, "/week\n",
      "         Room size: ", slot(object, "space"), " square meters\n",
      "    Max. admission: ", slot(object, "admission"), "€\n\n"
    ))
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,GEMA.liveclub-method
setMethod(
  "show",
  signature(object="GEMA.liveclub"),
  function(object){
    days <- slot(object, "days")
    dayName <- ifelse(days > 1, "days", "day")
    cat(paste0(
      "\n   Live club\n\n",
      "        Doors open: ", days, " ", dayName, "/month\n",
      "         Room size: ", slot(object, "space"), " square meters\n",
      "    Max. admission: ", slot(object, "admission"), "€\n",
      "    Recorded music: ", ifelse(isTRUE(slot(object, "plusMedium")), "yes", "no"), "\n\n"
    ))
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,GEMA.gig-method
setMethod(
  "show",
  signature(object="GEMA.gig"),
  function(object){
    guests <- slot(object, "guests")
    guestName <- ifelse(guests != 1, "guests", "guest")
    cat(paste0(
      "\n   Concert\n\n",
      "         Attendees: ", slot(object, "guests"), " ", guestName, "\n",
      "    Max. admission: ", slot(object, "admission"), "€\n",
      "    Gross receipts: ", slot(object, "gross"), "€\n",
      "     Add. revenues: ", ifelse(isTRUE(slot(object, "addIncome")), "yes", "no"), "\n",
      if(slot(object, "fee") > 0){paste0(
      "       Artists fee: ", slot(object, "fee"), "€\n"
      )} else {},
      if(slot(object, "production") > 0){paste0(
      "  Production costs: ", slot(object, "production"), "€\n"
      )} else {},
      "    Recorded music: ", ifelse(isTRUE(slot(object, "plusMedium")), "yes", "no"), "\n\n",
      if(any(
        isTRUE(slot(object, "charity")),
        isTRUE(slot(object, "rcs")),
        isTRUE(slot(object, "promoYoung"))
      )){
      paste0(
      "    The concert is",
        if(isTRUE(slot(object, "musicians"))){paste0(" \n      - performed by live musicians")
        } else {
          paste0(" \n      - performed using physical recording media")
        },
        if(isTRUE(slot(object, "charity"))){paste0(" \n      - charitable")} else {},
        if(isTRUE(slot(object, "rcs"))){paste0(" \n      - of religious, cultural or social character")} else {},
        if(isTRUE(slot(object, "promoYoung"))){paste0(" \n      - promoting young musicians")} else {},
      "\n\n"
      )},
      if(slot(object, "lumpSumContract") > 0){paste0(
      "   There is a lump sum contract conctract for at least ", slot(object, "lumpSumContract"), " concerts a year.\n\n"
      )}
    ))
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,GEMA.invoice_m_cd-method
setMethod(
  "show",
  signature(object="GEMA.invoice_m_cd"),
  function(object){
    invoice <- slot(object, "invoice")
    tariff <- slot(object, "tariff")
    reduction <- slot(object, "reduction")
    cat("\n Invoice:\n\n")
    print(invoice)
    cat("\n\n Actual tariff:\n\n")
    print(tariff)
    cat("\n\n Applied reduction (montly value):\n\n")
    print(reduction)
    cat("\n")
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,GEMA.invoice_u_t-method
setMethod(
  "show",
  signature(object="GEMA.invoice_u_t"),
  function(object){
    invoice <- slot(object, "invoice")
    cat("\n Invoice:\n\n")
    print(invoice)
    cat("\n")
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,GEMA.invoice_u_k-method
setMethod(
  "show",
  signature(object="GEMA.invoice_u_k"),
  function(object){
    invoice <- slot(object, "invoice")
    tariff <- slot(object, "tariff")
    reduction <- slot(object, "reduction")
    cat("\n Invoice:\n\n")
    print(invoice)
    cat("\n\n Actual tariff:\n\n")
    print(tariff)
    cat("\n\n Applied reduction:\n\n")
    print(reduction)
    cat("\n")
  }
)
