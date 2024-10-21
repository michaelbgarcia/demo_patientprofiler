#' app/view/ad_events

box::use(
  ggplot2[ggplot, aes, labs, element_text, theme, facet_wrap,
          geom_segment, geom_point,],
  shiny,
  stats[reorder,],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    shiny$plotOutput(ns("chart"))
  )
}

#' @export
server <- function(id, df) {
  shiny$moduleServer(id, function(input, output, session) {
    output$chart = shiny$renderPlot({
      shiny$validate(shiny$need(nrow(df()) > 0, "No AE data available for this patient."))
      gplot = # Create the line chart
        ggplot(df(), aes(y = reorder(AETERM, rev(ASTDT)))) +
        geom_segment(aes(x = ASTDY, xend = AENDY, yend = AETERM, color = AESEV), size = 1, show.legend = F) +
        geom_point(aes(x = ASTDY, color = AESEV), shape = 1, size = 3, show.legend = F) +
        geom_point(aes(x = AENDY, shape = AEOUT, color = AESEV), size = 3) +
        labs(
          x = "Analysis Event Day",
          y = "Adverse Event Term",
          color = "Severity",
          shape = "Outcome"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      gplot
    })
  })
}
