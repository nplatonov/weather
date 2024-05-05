require(shiny)
plutil::pluglibrary(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

# run a shiny app
# app <- system.file("examples/01_hello", package = "shiny")
app <- "app.Rmd"
runApp(app)

# once app has closed, display reactlog from shiny
shiny::reactlogShow()
