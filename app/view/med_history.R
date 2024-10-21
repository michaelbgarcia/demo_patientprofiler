#' app/view/med_history

box::use(
  dplyr[arrange, select,],
  reactable,
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    reactable$reactableOutput(ns("table"))
  )
}

#' @export
server <- function(id, df) {
  shiny$moduleServer(id, function(input, output, session) {
    output$table = reactable$renderReactable({
      shiny$req(df())
      dat = df() |> select(
        USUBJID, MHSEQ, MHTERM, MHLLT, MHDECOD, MHCAT, MHBODSYS, MHSEV
      ) |> 
        arrange(MHSEQ)
      reactable$reactable(
        dat, 
        defaultPageSize = 5,
        showPagination = TRUE,
        compact = TRUE,
        columns = list(
          MHSEQ = reactable$colDef(show = FALSE)
        )
      )
    })
  })
}
