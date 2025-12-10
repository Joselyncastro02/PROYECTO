rm(list = ls())
# LIBRERÍAS

library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(DT)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "northwind",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "123456"
)


#DATASET 
orders        <- tbl(con, "orders")
order_details <- tbl(con, "order_details")
customers     <- tbl(con, "customers")
products      <- tbl(con, "products")
categories    <- tbl(con, "categories")

colnames(order_details)
colnames(orders)
colnames(customers)
colnames(categories)
colnames(products)


#RENOMBRAR unit_price DE order_details


order_details2 <- order_details %>%
  rename(precio_unit = unit_price) %>%   # renombrar para evitar conflicto
  select(order_id, product_id, precio_unit, quantity)

#dataset
ventas_northwind <- orders %>%
  inner_join(order_details2, by = "order_id") %>%
  inner_join(customers,     by = "customer_id") %>%
  inner_join(products,      by = "product_id") %>%
  inner_join(categories,    by = "category_id") %>%
  select(order_id, order_date, country, 
         product_name, category_name,
         precio_unit, quantity) %>%
  collect() %>%
  mutate(
    total_venta = precio_unit * quantity,
    anio        = format(order_date, "%Y"),
    mes_num     = as.numeric(format(order_date, "%m")),
    mes         = month.abb[mes_num]
  )

View(ventas_northwind)

#ui

ui <- dashboardPage(
  dashboardHeader(title = "Northwind Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    ),
    
    selectInput("filtro_anio", "Seleccione un año:",
                choices = sort(unique(ventas_northwind$anio)),
                selected = max(ventas_northwind$anio))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "resumen",
              fluidRow(
                infoBoxOutput("box_ingresos", width = 6),
                infoBoxOutput("box_pedidos", width = 6)
              ),
              fluidRow(
                box(title = "Top 10 Países por Ventas",
                    width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("graf_paises")),
                
                box(title = "Top 10 Categorías",
                    width = 6, status = "warning", solidHeader = TRUE,
                    plotOutput("graf_categorias"))
              ),
              fluidRow(
                box(title = "Tendencia Mensual de Ventas",
                    width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("graf_mensual"))
              )
      ),
      
      tabItem(tabName = "datos",
              h3("Detalle Completo de Ventas"),
              box(width = 12, status = "primary",
                  dataTableOutput("tabla_detalle"))
      )
    )
  )
)

#inicar servidor 

server <- function(input, output, session) {
  
  # FILTRAR AÑO SELECCIONADO
  datos_filtrados <- reactive({
    ventas_northwind %>% 
      filter(anio == input$filtro_anio)
  })
  
  #kpi ingresos totales
 
  output$box_ingresos <- renderInfoBox({
    total <- sum(datos_filtrados()$total_venta, na.rm = TRUE)
    
    infoBox(
      title = "Ingresos Totales",
      value = paste0("$", format(round(total, 0), big.mark = ",")),
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
#kpi numero pedidos 
  # KPI: TOTAL PEDIDOS
  output$box_pedidos <- renderInfoBox({
    n_pedidos <- dplyr::n_distinct(datos_filtrados()$order_id)
    
    infoBox(
      title = "Pedidos Registrados",
      value = n_pedidos,
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  #top ventas
  
  # TOP 10 PAISES
  output$graf_paises <- renderPlot({
    datos_filtrados() %>%
      group_by(country) %>%
      summarise(total = sum(total_venta), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(country, total), y = total)) +
      geom_col(fill = "green") +
      coord_flip() +
      labs(x = "País", y = "Ventas ($)", title = "Top 10 Países por Ventas") +
      theme_minimal()
  })
  
  
  
  #top categorias 
  output$graf_categorias <- renderPlot({
    datos_filtrados() %>%
      group_by(category_name) %>%
      summarise(total = sum(total_venta), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(category_name, total), y = total, fill = category_name)) +
      geom_col() +
      coord_flip() +
      labs(x = "Categoría", y = "Ventas ($)", title = "Top 10 Categorías por Ventas") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #grafico mensual
  
  # TENDENCIA MENSUAL
  output$graf_mensual <- renderPlot({
    datos_filtrados() %>%
      group_by(mes_num, mes) %>%
      summarise(total = sum(total_venta), .groups = "drop") %>%
      arrange(mes_num) %>%
      ggplot(aes(x = mes, y = total, group = 1)) +
      geom_line(color = "pink", linewidth = 1.2) +
      geom_point(color = "blue", size = 3) +
      labs(x = "Mes", y = "Ventas ($)", title = "Tendencia Mensual de Ventas") +
      theme_minimal()
  })
  
  #ventas

  output$tabla_detalle <- renderDataTable({
    datos_filtrados() %>%
      select(order_id, order_date, country, product_name,
             category_name, precio_unit, quantity, total_venta)
  })
  
  
}
  
shinyApp(ui = ui, server = server)

