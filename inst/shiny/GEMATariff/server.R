library(shiny)
#.libPaths("~/R")
require(GEMATariff)

shinyServer(function(input, output){

  thisLocation <- reactive(
    if(input$tariffID == 'dancefloor'){
      return(dancefloor(
        space=input$space,
        days=input$daysw,
        admission=input$admission
      ))
    } else if(input$tariffID == 'liveclub'){
      return(liveclub(
        space=input$space,
        days=input$daysm,
        admission=input$admission,
        plusMedium=input$plusMedium
      ))
    } else if(input$tariffID == 'gig'){
      return(gig(
        gross=input$gross,
        guests=input$guests,
        admission=input$admission,
        musicians=as.logical(input$musicians),
        addIncome=input$addIncome,
        fee=input$fee,
        production=input$production,
        charity=input$charity,
        rcs=input$rcs,
        promoYoung=input$promoYoung,
        lumpSumContract=as.numeric(input$lumpSumContract),
        plusMedium=input$plusMedium
      ))
    } else {}
  )

  invoice.results <- reactive(
    if(input$tariffID == 'dancefloor'){
      return(GEMA_m_cd(
        dancefloor=thisLocation(),
        year=input$year2014,
        discount=input$discount
#         discount=1 - (input$discount / 100)
      ))
    } else if(input$tariffID == 'liveclub'){
      return(GEMA_u_t(
        liveclub=thisLocation(),
        discount=input$discount
#         discount=1 - (input$discount / 100)
      ))
    } else if(input$tariffID == 'gig'){
      return(GEMA_u_k(
        gig=thisLocation(),
        byGross=as.logical(input$byGross),
        year=input$year2014,
        discount=input$discount
#         discount=1 - (input$discount / 100)
      ))
    } else {}
  )
  
  output$tableTariff <- renderTable({
    tableTariff <- slot(invoice.results(), "tariff")
    if(input$lang == "de"){
      if(length(rownames(tableTariff)) == 5){
        rownames(tableTariff) <- c("GEMA", "GVL", "Netto", "USt", "Brutto")
      } else {
        rownames(tableTariff) <- c("GEMA", "GVL")
      }
      if(length(colnames(tableTariff)) == 3){
        colnames(tableTariff) <- c("Monat", "Quartal", "Jahr")
      } else {
        colnames(tableTariff) <- c("Tarif")
      }
    }
    return(tableTariff)
  })
  
  output$tableReduction <- renderTable({
    reduction <- slot(invoice.results(), "reduction")
    return(t(as.matrix(reduction)))
  })
  
  output$tableInvoice <- renderTable({
    tableInvoice <- slot(invoice.results(), "invoice")
    if(input$lang == "de"){
      if(length(colnames(tableInvoice)) == 3){
        colnames(tableInvoice) <- c("Monat", "Quartal", "Jahr")
      } else {
        colnames(tableInvoice) <- c("Tarif")
      }
      rownames(tableInvoice) <- c("GEMA", "GVL", "Netto", "USt", "Brutto")
    } else {}
    return(tableInvoice)
  })
  
  output$tariffPlot <- renderPlot({
    if(input$tariffID %in% c('dancefloor','gig')){
      plot(
        thisLocation(),
        year=input$year2014
      )
    } else {
      plot(1)
    }
  })

  ## i18n
  translations <- list(
    "GEMA tariff"=c(
      de="GEMA Tarif"
    ),
    "Doors open (per week)"=c(
      en="Doors open (per week):",
      de="Öffnungstage (pro Woche):"
    ),
    "Language"=c(
      de="Sprache"
    ),
    "Tariff"=c(
      de="Tarif"
    ),
    "tariffIDValues"=list(
      en=c("Club (DJ)", "Dance club wit live music", "Concert"),
      de=c("Club (DJ)", "Musikaufführung in Tanzlokalen", "Konzert")
    ),
    "Club (DJ)"=c(
      de="Club (DJ)"
    ),
    "Club (live)"=c(
      en="Dance club wit live music",
      de="Musikaufführung in Tanzlokalen"
    ),
    "Concert"=c(
      de="Livekonzert"
    ),
    "Dancefloor"=c(
      de="Dancefloor"
    ),
    "Live club"=c(
      de="Live-Club"
    ),
    "Doors open (per month)"=c(
      en="Doors open (per month):",
      de="Öffnungstage (pro Monat):"
    ),
    "Basis for calculation"=c(
      de="Berechnungsgrundlage"
    ),
    "Gross receipts"=c(
      en="Gross receipts (€):",
      de="Bruttoeinnahmen (€):"
    ),
    "Number of guests"=c(
      de="Anzahl Besucher"
    ),
    "Space (square meters)"=c(
      en="Space (square meters):",
      de="Raumgröße (m²):"
    ),
    "Maximum admission"=c(
      en="Maximum admission (€):",
      de="Maximaler Eintritt (€):"
    ),
    "Year (entry reduction)"=c(
      de="Jahr (Einführungsrabatt)"
    ),
    "Music is mostly performed"=c(
      de="Die Musik wird hauptsächlich"
    ),
    "Artits fee"=c(
      en="Artits fee (€):",
      de="Künstlergage (€):"
    ),
    "Production costs"=c(
      en="Production costs (€):",
      de="Produktionskosten (€):"
    ),
    "Additional revenue (advertising etc)"=c(
      en="Additional revenue (advertising etc.)",
      de="Weitere geldwerte Vorteile (Werbung etc.)"
    ),
    "Charity event"=c(
      de="Benefizveranstaltung"
    ),
    "Religious cultura social event"=c(
      en="Religious/cultura/social event",
      de="Religiöse/kulturelle/soziale Veranstaltung"
    ),
    "Promoting young musicians"=c(
      de="Nachwuchsförderung"
    ),
    "Public Invite only"=c(
      en="Public/Invite only",
      de="Öffentlich/nur geladene Gäste"
    ),
    "Lump sum contract"=c(
      de="Jahresrahmenvertrag"
    ),
    "Plus recorded music"=c(
      de="Zusätzliche Tonträgerwiedergabe"
    ),
    "Global discount"=c(
      en="Global discount:",
      de="Gesamtvertragsnachlass:"
    ),
    "Invoice"=c(
      de="Rechnung"
    ),
    "Reduction"=c(
      de="Nachlässe"
    ),
    "Reductions"=c(
      de="Nachlässe"
    ),
    "Surcharges"=c(
      de="Aufpreis"
    ),
    "Actual tariff"=c(
      de="Eigentlicher Tarif"
    ),
    "About"=c(
      de="Über"
    )
  )

  i18n_s <- function(text, lang=input$lang, trans=translations){
    this.trans <- trans[[text]]
    if(is.null(this.trans) | !lang %in% names(this.trans)){
      return(text)
    } else {
      return(this.trans[[lang]])
    }
  }

  for (thisTrans in names(translations)){
    if(!thisTrans %in% c("tariffIDValues", "lumpSumContractValues")){
      eval(parse(text=paste0("output$\"", thisTrans, "\" <- renderUI(i18n_s(\"", thisTrans,"\"))")))
    }
  }
  
#   output$tariffIDValues <- renderUI({
#     tariffIDValues <- c("dancefloor", "liveclub", "gig")
#     names(tariffIDValues) <- i18n_s("tariffIDValues")
#     selectInput(
#         "tariffID",
#         i18n_s("Tariff"),
#         choices=tariffIDValues
#       )
#   })
# 
#   output$lumpSumContractValues <- renderUI({
#     if(input$lang == "de"){
#       lumpSumContractValues <- c(
#         "kein Vertrag oder <= 15 Konzerte/Jahr"="0",
#         "> 15 Konzerte/Jahr"="16",
#         "> 30 Konzerte/Jahr"="31"
#       )
#     } else {
#       lumpSumContractValues <- c(
#         "no contract or <= 15 gigs/year"="0",
#         "> 15 gigs/year"="16",
#         "> 30 gigs/year"="31"
#       )
#     }
#     selectInput(
#       "lumpSumContract",
#       i18n_s("Lump sum contract"),
#       choices=lumpSumContractValues
#     )
#   })


})
