# 简化的测试应用
library(shiny)
library(leaflet)

# 简单的UI
ui <- fluidPage(
  titlePanel("Melbourne Vibe Finder - 测试版"),
  leafletOutput("map")
)

# 简单的服务器
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(144.9631, -37.8136, zoom = 14) |>
      addMarkers(
        lng = 144.9631, 
        lat = -37.8136, 
        popup = "墨尔本市中心"
      )
  })
}

# 启动应用
shinyApp(ui, server)
