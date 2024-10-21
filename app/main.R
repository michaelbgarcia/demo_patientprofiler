box::use(
  bsicons[bs_icon,],
  bslib,
  dplyr[filter,],
  pharmaverseadam[adae, adsl, admh,],
  sass,
  shiny,
)

box::use(
  app/view/info,
  app/view/med_history,
  app/view/ad_events,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$page_navbar(
    title = "Patient Profiler",
    underline = FALSE,
    sidebar = bslib$sidebar(
      shiny$selectInput(
        inputId = ns("select_pat_id"),
        label = "Select Patient",
        choices = unique(adsl$USUBJID),
        multiple = FALSE
      )
    ),
    theme = bslib$bs_theme(version = 5, bootswatch = "cerulean") |> 
      bslib$bs_add_rules(sass$sass_file("app/styles/main.scss")),
    bslib$nav_panel(
      title = "Summary",
      bslib$card(
        fill = FALSE,
        bslib$card_header(
          style = "display:flex; justify-content:space-between;align-items:center;",
          shiny$tags$div(
            style = "display:flex;align-items:center;",
            bs_icon("person-standing"), 
            "Patient Information"
          )
        ),
        info$ui(ns("info"))
      ),
      bslib$card(
        fill = FALSE,
        bslib$card_header(
          style = "display:flex;align-items:center;",
          bs_icon("clock-history"), 
          "Medical History"
        ),
        med_history$ui(ns("history"))
      ),
      bslib$card(
        fill = FALSE,
        bslib$card_header(
          style = "display:flex;align-items:center;",
          bs_icon("calendar-event"), 
          "Adverse Events"
        ),
        ad_events$ui(ns("events"))
      ),
      shiny$tags$div(
        style = "display:flex; justify-content:flex-end;padding:1em;",
        shiny$tags$span("Data from ", 
                        shiny$tags$a(href="https://pharmaverse.github.io/pharmaverseadam/index.html","{pharmaverseadam}"),
                        "package")
        )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    prep_values = shiny$reactiveValues(adsl_sub = NULL,
                                       admh_sub = NULL)
    shiny$observe({
      shiny$req(input$select_pat_id)
      prep_values$adsl_sub = adsl |> filter(USUBJID %in% input$select_pat_id)
    })
    
    shiny$observe({
      shiny$req(input$select_pat_id)
      prep_values$admh_sub = admh |> filter(USUBJID %in% input$select_pat_id)
    })
    
    shiny$observe({
      shiny$req(input$select_pat_id)
      prep_values$adae_sub = adae |> filter(USUBJID %in% input$select_pat_id)
    })
    
    info$server("info", df = shiny$reactive(prep_values$adsl_sub))
    med_history$server("history", df = shiny$reactive(prep_values$admh_sub))
    ad_events$server("events", df = shiny$reactive(prep_values$adae_sub))
  })
}