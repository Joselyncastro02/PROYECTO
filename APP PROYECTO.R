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

orders <- tbl(con, "orders")
order_details <- tbl(con, "order_details")
customers <- tbl(con, "customers")
products <- tbl(con, "products")
categories <- tbl(con, "categories")
employees <- tbl(con, "employees")
shippers <- tbl(con, "shippers")


ventas_pais <- orders %>%
  inner_join(order_details, by = "order_id") %>%
  inner_join(customers, by = "customer_id") %>%
  group_by(country) %>%
  summarise(ventas = sum(unit_price * quantity)) %>%
  arrange(desc(ventas)) %>%
  collect()

ventas_categoria <- order_details %>%
  inner_join(products, by = "product_id") %>%
  inner_join(categories, by = "category_id") %>%
  group_by(category_name) %>%
  summarise(ventas = sum(unit_price * quantity)) %>%
  arrange(desc(ventas)) %>%
  collect()



