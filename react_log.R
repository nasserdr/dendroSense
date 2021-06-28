library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

# run a shiny app
app <- system.file("./dendroViz/app", package = "shiny")
runApp(app)

# once app has closed, display reactlog from shiny
shiny::reactlogShow()
