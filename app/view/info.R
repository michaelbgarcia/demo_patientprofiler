#' app/view/info

box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    class = "info-grid",
    shiny$tags$div(
      class = "info-block",
      shiny$tags$h3("Participant Information"),
      shiny$tags$div(
        class = "table-container",
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Patient ID:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_id")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Study ID:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_study")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Randomization:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_randdt")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Arm:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_arm"))))
      )
    ),
    shiny$tags$div(
      class = "info-block",
      shiny$tags$h3("Demographics"),
      shiny$tags$div(
        class = "table-container",
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Age:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_age")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Sex:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_sex")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Race:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_race")))),
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Ethnicity:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_ethnic"))))
      )
    ),
    shiny$tags$div(
      class = "info-block",
      shiny$tags$h3("Site Information"),
      shiny$tags$div(
        class = "table-container",
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Site ID:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_site")))),        
        shiny$tags$div(class = "table-row", shiny$tags$div(class = "table-cell","Country:"), shiny$tags$div(class = "table-cell", shiny$textOutput(ns("out_country")))),
      )
    )
  )
}

#' @export
server <- function(id, df) {
  shiny$moduleServer(id, function(input, output, session) {
    
    output$out_id = shiny$renderText({
      shiny$req(df())
      df()$SUBJID[[1]]
    })
    output$out_study = shiny$renderText({
      shiny$req(df())
      df()$STUDYID[[1]]
    })
    output$out_randdt = shiny$renderText({
      shiny$req(df())
      as.character(df()$RANDDT[[1]])
    })
    output$out_arm = shiny$renderText({
      shiny$req(df())
      df()$ARM[[1]]
    })
    output$out_age = shiny$renderText({
      shiny$req(df())
      df()$AGE[[1]]
    })
    output$out_sex = shiny$renderText({
      shiny$req(df())
      df()$SEX[[1]]
    })
    output$out_race = shiny$renderText({
      shiny$req(df())
      df()$RACE[[1]]
    })
    output$out_ethnic = shiny$renderText({
      shiny$req(df())
      df()$ETHNIC[[1]]
    })
    output$out_country = shiny$renderText({
      shiny$req(df())
      df()$COUNTRY[[1]]
    })
    output$out_site = shiny$renderText({
      shiny$req(df())
      df()$SITEID[[1]]
    })
  })
}
