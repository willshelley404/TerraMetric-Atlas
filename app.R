# ─────────────────────────────────────────────────────────────────────────────
# app.R — EconPulse AI Entry Point
# ─────────────────────────────────────────────────────────────────────────────
# Run with: shiny::runApp("app.R")
# Or from the project directory: shiny::runApp()
# ─────────────────────────────────────────────────────────────────────────────

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
