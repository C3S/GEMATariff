library(shiny)

i18n <- function(text){uiOutput(text)}

shinyUI(
  fluidPage(
    titlePanel("GEMATariff"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "tariffID",
          i18n("Tariff"),
          choices=c( # found no way to translate these yet...
            "Club (DJ)"="dancefloor",
            "Musikaufführung in Tanzlokalen"="liveclub",
            "Konzert"="gig"
          )
        ),
        conditionalPanel("input.tariffID == 'dancefloor'",
          h4(i18n("Dancefloor")),
          sliderInput("daysw", i18n("Doors open (per week)"), min=1, max=7, value=1, step=1)
        ),
        conditionalPanel("input.tariffID == 'liveclub'",
          h4(i18n("Live club")),
          sliderInput("daysm", i18n("Doors open (per month)"), min=1, max=30, value=1, step=1)
        ),
        conditionalPanel("input.tariffID == 'gig'",
          h4(i18n("Concert")),
          selectInput("byGross", i18n("Basis for calculation"),
            choices=c(
              "Bruttoeinnahmen"="TRUE",
              "Geschätzt aus Maximaleinnahmen"="FALSE"
            )
          )
        ),
        conditionalPanel("input.tariffID == 'gig' && input.byGross == 'TRUE'",
          numericInput("gross", i18n("Gross receipts"), 0, min=0)
        ),
        conditionalPanel("input.tariffID == 'gig' && input.byGross == 'FALSE'",
          numericInput("guests", i18n("Number of guests"), 150, min=0)
        ),
        conditionalPanel("input.tariffID == 'liveclub' || input.tariffID == 'dancefloor'",
          sliderInput("space", i18n("Space (square meters)"), min=100, max=2000, value=100, step=100)
        ),
        conditionalPanel("input.tariffID != 'gig' || input.byGross == 'FALSE'",
          numericInput("admission", i18n("Maximum admission"), 0, min=0)
        ),
        conditionalPanel("input.tariffID == 'gig' && input.public",
          numericInput("fee", i18n("Artits fee"), 0, min=0),
          numericInput("production", i18n("Production costs"), 0, min=0)
        ),
        # conditionalPanel("input.tab == 'fee' || input.tab == 'feeGEMA'",
        conditionalPanel("input.tariffID == 'gig'",
          selectInput("musicians", i18n("Music is mostly performed"),
            choices=c(
#               "by live musicians"=TRUE,
#               "using recorded music"=FALSE
              "von Live-Musikern gespielt"="TRUE",
              "von Tonträgern gespielt"="FALSE"
            )
          )
        ),
        h4(i18n("Reductions")),
        conditionalPanel("input.tariffID == 'dancefloor' || input.tariffID == 'gig'",
          sliderInput("year2014", i18n("Year (entry reduction)"), min=2014, max=2022, value=as.numeric(format(Sys.time(), "%Y")), step=1)
        ),
        checkboxInput("discount", i18n("Global discount"), FALSE),
#         sliderInput("discount", i18n("Global discount"), min=0, max=100, value=0, step=1),
        conditionalPanel("input.tariffID == 'gig'",
          checkboxInput("charity", i18n("Charity event"), FALSE),
          checkboxInput("rcs", i18n("Religious cultura social event"), FALSE),
          checkboxInput("promoYoung", i18n("Promoting young musicians"), FALSE),
          checkboxInput("public", i18n("Public Invite only"), FALSE),
#           i18n("lumpSumContractValues") #
          selectInput(
            "lumpSumContract",
            i18n("Lump sum contract"),
            choices=c( # found no way to translate these yet...
#               "no contract or <= 15 gigs/year"="0",
#               "> 15 gigs/year"="16",
#               "> 30 gigs/year"="31"
              "kein Vertrag oder <= 15 Konzerte/Jahr"="0",
              "> 15 Konzerte/Jahr"="16",
              "> 30 Konzerte/Jahr"="31"
            )
          )
        ),
        conditionalPanel("input.tariffID == 'liveclub' || input.tariffID == 'gig'",
          h4(i18n("Surcharges")),
          checkboxInput("plusMedium", i18n("Plus recorded music"), FALSE)
        ),
        conditionalPanel("input.tariffID == 'gig'",
          checkboxInput("addIncome", i18n("Additional revenue (advertising etc)"), FALSE)
        ),
        selectInput(
          "lang",
          i18n("Language"),
          choices=c(
            "Deutsch"="de",
            "English"="en"
          )
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Tariff",
            conditionalPanel("input.tariffID == 'dancefloor'",
              br(),
              p(a(href="https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_m_cd.pdf", target="_blank", "M-CD II. 2 Tonträgerwiedergabe in Discotheken (PDF)"))
            ),
            conditionalPanel("input.tariffID == 'liveclub'",
              br(),
              p(a(href="https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_u_t.pdf", target="_blank", "U-T I Musikaufführungen in Tanzlokalen (PDF)"))
            ),
            conditionalPanel("input.tariffID == 'gig'",
              br(),
              p(a(href="https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_u_k.pdf", target="_blank", "U-K I Wiedergaben mit Musikern"))
            ),
            h3(i18n("Invoice")),
            tableOutput("tableInvoice"),
            conditionalPanel("input.tariffID == 'dancefloor' || input.tariffID == 'gig'",
              plotOutput("tariffPlot")
            ),
            h4(i18n("Reduction")),
            tableOutput("tableReduction"),
            h4(i18n("Actual tariff")),
            tableOutput("tableTariff"),
            value="invoiceDancefloor"
          ),
          tabPanel(i18n("About"),
            h3("The GEMATariff package"),
            p(
              "This ", a("R", href="https://www.r-project.org", target="_blank"), " package was developed to get a better understanding of the tariff system of the German collective rights society ", a("GEMA", href="https://www.gema.de", target="_blank"), ".",
              "It is intended to be useful as a research tool, and not for any practical application beyond that."
            ),
            p(
              "Some of the tariffs are quite complex. Allthough we implemented everything to the best of our abilities, we do not warranty the correctness of these calculations.",
              "Also, the tariffs are constantly subject to change, and while you read this, some of these changes might not have been implemented yet."
            ),
            p(
              "If you need reliable feed back on these tariffs, you should try the ", a("official GEMA online calculator", href="https://online.gema.de/aidaos/", target="_blank"), " instead."
            ),
            h4("Free software"),
            p(
              "That being said, GEMATariff is free software: you can redistribute it and/or modify it under the terms of the ", a("GNU Affero General Public License", href="https://www.gnu.org/licenses/licenses.html#AGPL", target="_blank"), " as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.",
              "GEMATariff is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ", a("GNU Affero General Public License", href="https://www.gnu.org/licenses/licenses.html#AGPL", target="_blank"), " for more details."
            ),
            p(
              a("Get the source code.", href="https://", target="_blank", style="font-weight:bold;")
            ),
            h4("Acknowledgments"), 
            p("We would like to thank the GEMA staff for kindly testing our calculator and giving valuable feedback.")
          ),
          id="tab"
        ),
        div("GEMATariff ©2016 ", a(href="https://www.c3s.cc", target="_blank", "C3S"), style="text-align:center;font-size:70%;padding-top:15px;padding-bottom:15px;")
      )
    )
  )
)
