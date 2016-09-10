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

#' Plot method for objects of class GEMA.dancefloor
#'
#' Plot method for S4 objects of classes of the \code{GEMATariff} package,
#' plots the development of the new club tariff from 2014 to 2022.
#' 
#' @param x An object of class
#'    \code{GEMA.dancefloor} or
#'    \code{GEMA.gig}.
#' @param y From the generic \code{plot} function, ignored.
#' @param year Integer between 2014 and 2022, draws a line at the given year.
#' @param xlab See \code{\link[graphics]{plot}}.
#' @param ylab See \code{\link[graphics]{plot}}.
#' @param main See \code{\link[graphics]{plot}}.
#' @param ... Additional options given to \code{plot}.
#' @keywords methods plot
#' @aliases plot,-methods plot,GEMA.dancefloor-method
#' @export
#' @docType methods
#' @rdname plot-methods
setMethod("plot", signature(x="GEMA.dancefloor", y="missing"), function(
  x,
  year=as.numeric(format(Sys.time(), "%Y")),
  xlab="Year",
  ylab="Tariff (€)",
  main="Development of the GEMA tariff",
  ...){
    space <- slot(x, "space")
    days <- slot(x, "days")
    admission <- slot(x, "admission")
    dayName <- ifelse(days > 1, "days", "day")
    subtitle <- paste0("Dancefloor with ", space, "m², open ", days, " ", dayName, "/week, admission ", admission, "€")
    
    allYears <- 2014:2022
    allYearResults <- lapply(
      allYears,
      function(year){
        GEMA_m_cd(
          dancefloor=x,
          year=year
        )
      }
    )
    allYearResultsOld <- lapply(
      allYears,
      function(year){
        GEMA_m_u_iii(
          dancefloor=x
        )
      }
    )
    invoiceMonthly <- fetchMatrix(allYearResults, "invoice", "month")
    invoiceQuarterly <- fetchMatrix(allYearResults, "invoice", "quarter")
    invoiceYearly <- fetchMatrix(allYearResults, "invoice", "year")
    invoiceMonthlyOld <- fetchMatrix(allYearResultsOld, "invoice", "month")
    invoiceQuarterlyOld <- fetchMatrix(allYearResultsOld, "invoice", "quarter")
    invoiceYearlyOld <- fetchMatrix(allYearResultsOld, "invoice", "year")
    
    lineCols <- c(
      yn="darkred",
      qn="red",
      mn="darksalmon",
      yo="darkolivegreen",
      qo="olivedrab",
      mo="olivedrab3",
      rd="blue"
    )
    
    ymax <- max(invoiceYearly["GEMA",], invoiceYearlyOld["GEMA",])
    
    plot(
      invoiceYearly["GEMA",],
      col=lineCols["yn"],
      type="l",
      xlab=xlab,
      ylab=ylab,
      ylim=c(0,ymax),
      main=main,
      sub=subtitle,
      xaxt="n",
      lwd=2,
      ...
    )
    lines(
      invoiceMonthlyOld["GEMA",],
      lty="dotted",
      col=lineCols["mo"],
      lwd=2
    )
    lines(
      invoiceQuarterlyOld["GEMA",],
      lty="dotted",
      col=lineCols["qo"],
      lwd=2
    )
    lines(
      invoiceYearlyOld["GEMA",],
      lty="dotted",
      col=lineCols["yo"],
      lwd=2
    )
    lines(
      invoiceQuarterly["GEMA",],
      col=lineCols["qn"],
      lwd=2
    )
    lines(
      invoiceMonthly["GEMA",],
      col=lineCols["mn"],
      lwd=2
    )
    xyear <- which(allYears %in% year)
    yyear <- ymax / 2 #invoiceYearly["GEMA",xyear] * .9
    redLabel <- paste0(
      ifelse(
        slot(allYearResults[[xyear]], "reduction")[["launch"]] < 0,
        reduction_entry(year, tariff="m_cd") * 100,
        0
      ),
      "%"
    )
    abline(v=xyear, col=lineCols["rd"])
    text(
      x=xyear,
      y=yyear,
      labels=redLabel,
      pos=ifelse(xyear < length(allYears), 4, 2),
      col=lineCols["rd"]
    )
    axis(1, at=c(1:ncol(invoiceMonthly)), labels=colnames(invoiceMonthly))
    legend(
      "topleft",
      legend=c("yearly", "quarterly", "monthly", "yearly (2012)", "quarterly (2012)", "monthly (2012)", "reduction"),
      col=lineCols,
      lty=c(1,1,1,3,3,3,1),
      lwd=2
    )
  }
)

#' @export
#' @aliases plot,GEMA.gig-method
#' @docType methods
#' @rdname plot-methods
setMethod("plot", signature(x="GEMA.gig", y="missing"), function(
  x,
  year=as.numeric(format(Sys.time(), "%Y")),
  xlab="Year",
  ylab="Tariff (€)",
  main="Development of the GEMA tariff",
  ...){
    guests <- slot(x, "guests")
    guestName <- ifelse(guests > 1, "guests", "guest")
    admission <- slot(x, "admission")
    subtitle <- paste0("Concert with ", guests, " ", guestName, ", admission ", admission, "€")

    year <- min(2022, year)
    allYears <- 2014:2022
    allYearResults <- lapply(
      allYears,
      function(year){
        GEMA_u_k(
          gig=x,
          year=year
        )
      }
    )
    allYearResultsOld <- lapply(
      allYears,
      function(year){
        GEMA_u_k(
          gig=x,
          year=2014
        )
      }
    )
    invoiceTariff <- fetchMatrix(allYearResults, "invoice", "tariff", names=allYears)
    invoiceTariffOld <- fetchMatrix(allYearResultsOld, "invoice", "tariff", names=allYears)
    
    lineCols <- c(
      tn="darkred",
      to="darkolivegreen",
      rd="blue"
    )
    
    ymax <- max(invoiceTariff["GEMA",], invoiceTariffOld["GEMA",])
    
    plot(
      invoiceTariff["GEMA",],
      col=lineCols["tn"],
      type="l",
      xlab=xlab,
      ylab=ylab,
      ylim=c(0,ymax),
      main=main,
      sub=subtitle,
      xaxt="n",
      lwd=2,
      ...
    )
    lines(
      invoiceTariffOld["GEMA",],
      lty="dotted",
      col=lineCols["to"],
      lwd=2
    )
    xyear <- which(allYears %in% year)
    yyear <- ymax / 2 # invoiceTariff["GEMA",xyear] * .9
    redLabel <- paste0(
      if(year > 2015){
        if(guests > 2000){
          paste0(reduction_entry(year, tariff="u_k_gt2000") * 100)
        } else {
          paste0(reduction_entry(year, tariff="u_k_le2000") * 100)
        }
      } else if(year == 2015){
        # this year needs more special treatment...
        if(guests > 15000){
          paste0(7.65)
        } else if(guests > 2000){
          paste0(reduction_entry(year=year, tariff="u_k_gt2000") * 100)
        } else {
          paste0(reduction_entry(year=year, tariff="u_k_le2000") * 100)
        }
      } else if(year > 2012){
        if(year == 2013){
          paste0(category_steps(value=guests, steps=c(2000,15000), names=c(4.5,5.2,6.85)))
        } else {
          paste0(category_steps(value=guests, steps=c(2000,15000), names=c(5,7.2,7.65)))
        }
      } else {
        stop(simpleError("This tariff can't be calculated for years before 2013!"))
      },
      "%"
    )
    abline(v=xyear, col=lineCols["rd"])
    text(
      x=xyear,
      y=yyear,
      labels=redLabel,
      pos=ifelse(xyear < length(allYears), 4, 2),
      col=lineCols["rd"]
    )
    axis(1, at=c(1:ncol(invoiceTariff)), labels=colnames(invoiceTariff))
    legend(
      "topleft",
      legend=c("tariff", "tariff (2014)", "percentage of gross income"),
      col=lineCols,
      lty=c(1,3,1),
      lwd=2
    )
  }
)


# internal stuff
fetchMatrix <- function(list, slot, period, names=2014:2022){
  thisMatrix <- sapply(
    list,
    function(x){
      if(slot %in% "reduction"){
        slot(x, slot)[,period]
      } else {
        slot(x, slot)[,period]
      }
    }
  )
  colnames(thisMatrix) <- as.character(names)
  return(thisMatrix)
}
