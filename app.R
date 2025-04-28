# main app

source("global.R") # Data loading, cleaning, model training, global variables
source("ui.R") # User interface definition
source("server.R") # Server logic definition

# Run the Shiny application
shinyApp(ui = ui, server = server)
