rmarkdown::run("app.Rmd"
              ,render_args=list(output_options=list(),quiet=TRUE)
              ,shiny_args=list(launch.browser=TRUE,port=9992)
             # ,quiet=!TRUE
              )
