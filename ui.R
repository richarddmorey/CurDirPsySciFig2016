require(shiny)


reactiveSvg <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

textInput3<-function (inputId, label, value = "",...) 
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,...))
}


shinyUI(
  navbarPage(
    title="Figures, Wagenmakers, Morey, & Lee",
    tabPanel(title="Figure 1",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          HTML('<h4>Prior</h4>'),
          HTML('Bob\'s IQ (normal)'),
          sliderInput("mean.prior", "mean", 
                      min=60, max=120, value=75,step= 1),
          sliderInput("sd.prior", "std. dev.", 
                      min=5, max=15, value=12,step= 1),
          sliderInput("sd.y", "TestSD (uniform)",
                      min = 0, max = 15, value = c(5,15)),
          HTML('<hr><h4>Data</h4>'),
          HTML('Separate numbers with commas.'),
          textInput("y", "Scores", value = "73,67,79"),
          HTML('<hr><h4>Settings</h4>'),
          sliderInput("x.lim", "x limits",
                      min = 40, max = 150, value = c(40,115))
        ),
        mainPanel(
          htmlOutput(outputId="svg.grid")
        )
      )
    ),
    tabPanel(title="Fig. 1 expl.",
      withMathJax(),
      uiOutput(outputId="summaries")
    ),
    tabPanel(title="Figure 3",
        plotOutput(outputId="svg.grid2")
    ),
    tabPanel(title="Fig. 3 expl.",
             withMathJax(),
             helpText('This is explanation for Figure 3.')
    )
  )
)
