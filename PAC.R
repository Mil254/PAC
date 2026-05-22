# ============================================================
#  DASHBOARD GAPMINDER PROFESIONAL
#  Paquetes requeridos: shiny, shinydashboard, ggplot2, dplyr,
#                       gapminder, plotly, DT, scales, leaflet, maps
#
#  Para instalar todos los paquetes, ejecuta en la consola de R:
#  install.packages(c("shiny","shinydashboard","ggplot2","dplyr",
#                     "gapminder","plotly","DT","scales","leaflet","maps"))
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(gapminder)
library(plotly)
library(DT)
library(scales)
library(leaflet)
library(maps)


data("gapminder")

# ── Paleta de colores por continente ───────────────────────
continent_colors <- c(
  "Africa"   = "#E74C3C",
  "Americas" = "#3498DB",
  "Asia"     = "#F39C12",
  "Europe"   = "#2ECC71",
  "Oceania"  = "#9B59B6"
)

# ── Helper: etiqueta legible de variable ───────────────────
var_label <- function(v) switch(v,
                                lifeExp   = "Expectativa de Vida (años)",
                                gdpPercap = "PIB per Cápita (USD)",
                                pop       = "Población"
)

# ── Helper: tema oscuro para Plotly ────────────────────────
dark_theme <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = "#cccccc"),
    xaxis = list(gridcolor = "#2a2a4a", zerolinecolor = "#444444"),
    yaxis = list(gridcolor = "#2a2a4a", zerolinecolor = "#444444"),
    legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#cccccc"))
  )
}


# ── Traducción completa de países al español ─────────────────
paises_es <- c(
  "Afghanistan" = "Afganistán",
  "Albania" = "Albania",
  "Algeria" = "Argelia",
  "Angola" = "Angola",
  "Argentina" = "Argentina",
  "Australia" = "Australia",
  "Austria" = "Austria",
  "Bahrain" = "Baréin",
  "Bangladesh" = "Bangladés",
  "Belgium" = "Bélgica",
  "Benin" = "Benín",
  "Bolivia" = "Bolivia",
  "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
  "Botswana" = "Botsuana",
  "Brazil" = "Brasil",
  "Bulgaria" = "Bulgaria",
  "Burkina Faso" = "Burkina Faso",
  "Burundi" = "Burundi",
  "Cambodia" = "Camboya",
  "Cameroon" = "Camerún",
  "Canada" = "Canadá",
  "Central African Republic" = "República Centroafricana",
  "Chad" = "Chad",
  "Chile" = "Chile",
  "China" = "China",
  "Colombia" = "Colombia",
  "Comoros" = "Comoras",
  "Congo, Dem. Rep." = "República Democrática del Congo",
  "Congo, Rep." = "República del Congo",
  "Costa Rica" = "Costa Rica",
  "Cote d'Ivoire" = "Costa de Marfil",
  "Croatia" = "Croacia",
  "Cuba" = "Cuba",
  "Czech Republic" = "República Checa",
  "Denmark" = "Dinamarca",
  "Djibouti" = "Yibuti",
  "Dominican Republic" = "República Dominicana",
  "Ecuador" = "Ecuador",
  "Egypt" = "Egipto",
  "El Salvador" = "El Salvador",
  "Equatorial Guinea" = "Guinea Ecuatorial",
  "Eritrea" = "Eritrea",
  "Ethiopia" = "Etiopía",
  "Finland" = "Finlandia",
  "France" = "Francia",
  "Gabon" = "Gabón",
  "Gambia" = "Gambia",
  "Germany" = "Alemania",
  "Ghana" = "Ghana",
  "Greece" = "Grecia",
  "Guatemala" = "Guatemala",
  "Guinea" = "Guinea",
  "Guinea-Bissau" = "Guinea-Bisáu",
  "Haiti" = "Haití",
  "Honduras" = "Honduras",
  "Hong Kong, China" = "Hong Kong",
  "Hungary" = "Hungría",
  "Iceland" = "Islandia",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Iran" = "Irán",
  "Iraq" = "Irak",
  "Ireland" = "Irlanda",
  "Israel" = "Israel",
  "Italy" = "Italia",
  "Jamaica" = "Jamaica",
  "Japan" = "Japón",
  "Jordan" = "Jordania",
  "Kenya" = "Kenia",
  "Korea, Dem. Rep." = "Corea del Norte",
  "Korea, Rep." = "Corea del Sur",
  "Kuwait" = "Kuwait",
  "Lebanon" = "Líbano",
  "Lesotho" = "Lesoto",
  "Liberia" = "Liberia",
  "Libya" = "Libia",
  "Madagascar" = "Madagascar",
  "Malawi" = "Malaui",
  "Malaysia" = "Malasia",
  "Mali" = "Malí",
  "Mauritania" = "Mauritania",
  "Mauritius" = "Mauricio",
  "Mexico" = "México",
  "Mongolia" = "Mongolia",
  "Montenegro" = "Montenegro",
  "Morocco" = "Marruecos",
  "Mozambique" = "Mozambique",
  "Myanmar" = "Myanmar",
  "Namibia" = "Namibia",
  "Nepal" = "Nepal",
  "Netherlands" = "Países Bajos",
  "New Zealand" = "Nueva Zelanda",
  "Nicaragua" = "Nicaragua",
  "Niger" = "Níger",
  "Nigeria" = "Nigeria",
  "Norway" = "Noruega",
  "Oman" = "Omán",
  "Pakistan" = "Pakistán",
  "Panama" = "Panamá",
  "Paraguay" = "Paraguay",
  "Peru" = "Perú",
  "Philippines" = "Filipinas",
  "Poland" = "Polonia",
  "Portugal" = "Portugal",
  "Puerto Rico" = "Puerto Rico",
  "Reunion" = "Reunión",
  "Romania" = "Rumanía",
  "Rwanda" = "Ruanda",
  "Sao Tome and Principe" = "Santo Tomé y Príncipe",
  "Saudi Arabia" = "Arabia Saudita",
  "Senegal" = "Senegal",
  "Serbia" = "Serbia",
  "Sierra Leone" = "Sierra Leona",
  "Singapore" = "Singapur",
  "Slovak Republic" = "República Eslovaca",
  "Slovenia" = "Eslovenia",
  "Somalia" = "Somalia",
  "South Africa" = "Sudáfrica",
  "Spain" = "España",
  "Sri Lanka" = "Sri Lanka",
  "Sudan" = "Sudán",
  "Swaziland" = "Esuatini",
  "Sweden" = "Suecia",
  "Switzerland" = "Suiza",
  "Syria" = "Siria",
  "Taiwan" = "Taiwán",
  "Tanzania" = "Tanzania",
  "Thailand" = "Tailandia",
  "Togo" = "Togo",
  "Trinidad and Tobago" = "Trinidad y Tobago",
  "Tunisia" = "Túnez",
  "Turkey" = "Turquía",
  "Uganda" = "Uganda",
  "United Kingdom" = "Reino Unido",
  "United States" = "Estados Unidos",
  "Uruguay" = "Uruguay",
  "Venezuela" = "Venezuela",
  "Vietnam" = "Vietnam",
  "West Bank and Gaza" = "Cisjordania y Gaza",
  "Yemen, Rep." = "Yemen",
  "Zambia" = "Zambia",
  "Zimbabwe" = "Zimbabue"
)

# ============================================================
#  INTERFAZ DE USUARIO (UI)
# ============================================================
ui <- dashboardPage(
  skin = "black",
  
  # ── Cabecera ─────────────────────────────────────────────
  dashboardHeader(
    title = tags$b("📊 Gapminder Analytics"),
    titleWidth = 280
  ),
  
  # ── Barra lateral ─────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("🌍  Visión Global",       tabName = "overview",      icon = icon("globe")),
      menuItem("📈  Tendencias",          tabName = "trends",        icon = icon("chart-line")),
      menuItem("🫧  Burbujas Animadas",   tabName = "bubble",        icon = icon("circle")),
      menuItem("📊  Distribuciones",      tabName = "distributions", icon = icon("chart-bar")),
      menuItem("🔗  Correlaciones",       tabName = "correlations",  icon = icon("arrows-to-circle")),
      menuItem("🧮  Modelo Estadístico",  tabName = "model",         icon = icon("calculator")),
      menuItem("🗃️  Datos Completos",     tabName = "data",          icon = icon("table")),
      menuItem("🏆 Ranking y Mapa",       tabName = "ranking_mapa",  icon = icon("map-marked-alt"))
    ),
    hr(),
    # Filtros globales
    tags$div(style = "padding: 0 14px;",
             tags$small(style = "color:#999;", "⚙️ Filtros Globales"),
             br(),
             selectInput("g_continent", "Continente:",
                         choices  = c("Todos" = "All", levels(gapminder$continent)),
                         selected = "All"),
             sliderInput("g_year", "Rango de Años:",
                         min = 1952, max = 2007,
                         value = c(1952, 2007),
                         step = 5, sep = "", ticks = FALSE)
    )
  ),
  
  # ── Cuerpo ────────────────────────────────────────────────
  dashboardBody(
    
    # CSS personalizado (tema oscuro profesional)
    tags$head(tags$style(HTML("
      body, .wrapper { background-color: #0f0f1a !important; }
      .content-wrapper, .right-side { background-color: #0f0f1a; color: #e0e0e0; }
      .main-sidebar { background-color: #13131f !important; }
      .sidebar-menu > li > a { color: #aaaaaa !important; font-size: 13.5px; }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li:hover  > a { background-color: #1e3a5f !important; color: #ffffff !important; }
      .treeview-menu > li > a { color: #999 !important; }
      .box {
        background-color: #161626 !important;
        border-radius: 8px;
        border-top: 3px solid;
        box-shadow: 0 2px 12px rgba(0,0,0,0.5);
      }
      .box-header .box-title { color: #dddddd !important; font-size: 15px; }
      .box-body { color: #cccccc; }
      .box.box-primary  { border-top-color: #3498db; }
      .box.box-success  { border-top-color: #2ecc71; }
      .box.box-warning  { border-top-color: #f39c12; }
      .box.box-info     { border-top-color: #9b59b6; }
      .box.box-danger   { border-top-color: #e74c3c; }
      .small-box { border-radius: 8px; }
      .small-box h3 { font-size: 2rem !important; }
      .info-box { border-radius: 8px; }
      hr { border-color: #2a2a3e; }
      .selectize-input  { background-color: #1e1e30 !important; color: #ccc !important; border-color: #333 !important; }
      .selectize-dropdown { background-color: #1e1e30 !important; color: #ccc !important; }
      .irs-bar, .irs-bar-edge { background: #3498db !important; border-color: #3498db !important; }
      .irs-single { background: #3498db !important; }
      .irs-grid-text { color: #888 !important; }
      table.dataTable tbody tr  { background-color: #1a1a2c !important; color: #ccc !important; }
      table.dataTable tbody tr:hover { background-color: #1e3a5f !important; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select { background: #1e1e30; color: #ccc; border: 1px solid #333; }
      .dataTables_wrapper { color: #ccc; }
      .page-header { color: #e0e0e0; border-color: #2a2a3e; }
      pre { background: #0e0e1c; color: #aaffaa; border: 1px solid #2a2a3e; border-radius: 5px; }
      .shiny-input-container label { color: #bbb; }
    "))),
    
    tabItems(
      
      # ════════════════════════════════════════════════════
      # TAB 1 — VISIÓN GLOBAL
      # ════════════════════════════════════════════════════
      tabItem(tabName = "overview",
              fluidRow(
                tags$div(style = "padding: 10px 15px 5px;",
                         tags$h2("🌍 Visión Global del Desarrollo Humano",
                                 style = "color:#e0e0e0; margin-bottom:2px;"),
                         tags$p("Indicadores clave del bienestar humano según la base de datos Gapminder.",
                                style = "color:#888;")
                )
              ),
              # KPI Cards
              fluidRow(
                valueBoxOutput("kpi_paises",    width = 3),
                valueBoxOutput("kpi_vida",      width = 3),
                valueBoxOutput("kpi_gdp",       width = 3),
                valueBoxOutput("kpi_poblacion", width = 3)
              ),
              fluidRow(
                box(title = "Expectativa de Vida Promedio por Continente",
                    status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("ov_vida_cont", height = "300px")),
                box(title = "PIB per Cápita Promedio por Continente",
                    status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("ov_gdp_cont", height = "300px"))
              ),
              fluidRow(
                box(title = "Evolución de la Expectativa de Vida por Continente (1952–2007)",
                    status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("ov_tendencia", height = "320px"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 2 — TENDENCIAS POR PAÍS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "⚙️ Configuración", status = "warning",
                    solidHeader = TRUE, width = 3,
                    selectInput("tr_paises", "País(es):",
                                choices  = sort(unique(as.character(gapminder$country))),
                                selected = c("Peru", "Chile", "Argentina", "Mexico"),
                                multiple = TRUE, selectize = TRUE),
                    selectInput("tr_variable", "Variable:",
                                choices = c(
                                  "Expectativa de Vida (años)" = "lifeExp",
                                  "PIB per Cápita (USD)"       = "gdpPercap",
                                  "Población"                  = "pop"
                                ),
                                selected = "lifeExp"),
                    hr(),
                    tags$small(style="color:#999;",
                               "💡 Selecciona varios países para compararlos lado a lado.")
                ),
                box(title = "Evolución Temporal por País",
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("tr_grafico", height = "420px"))
              ),
              fluidRow(
                box(title = "📋 Tabla Comparativa de Tendencias",
                    status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("tr_tabla"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 3 — BURBUJAS ANIMADAS (Hans Rosling)
      # ════════════════════════════════════════════════════
      tabItem(tabName = "bubble",
              fluidRow(
                tags$div(style = "padding: 10px 15px 5px;",
                         tags$h2("🫧 Gráfico de Burbujas Animado",
                                 style = "color:#e0e0e0; margin-bottom:2px;"),
                         tags$p("Cada burbuja es un país. Su tamaño representa la población. El eje X es PIB per cápita y el eje Y, expectativa de vida. Presiona ▶ Play para ver la evolución 1952–2007.",
                                style = "color:#888;")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = FALSE, width = 12,
                    plotlyOutput("bb_burbujas", height = "580px"))
              ),
              fluidRow(
                infoBox("💡 Insight Principal", width = 12,
                        "A medida que el PIB per cápita crece, la expectativa de vida mejora. Los países del Este Asiático (China, Corea del Sur) muestran los saltos más dramáticos entre 1952 y 2007.",
                        icon = icon("lightbulb"), color = "yellow", fill = TRUE)
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 4 — DISTRIBUCIONES
      # ════════════════════════════════════════════════════
      tabItem(tabName = "distributions",
              fluidRow(
                box(title = "⚙️ Configuración", status = "warning",
                    solidHeader = TRUE, width = 3,
                    selectInput("di_variable", "Variable:",
                                choices = c(
                                  "Expectativa de Vida" = "lifeExp",
                                  "PIB per Cápita"      = "gdpPercap",
                                  "Población"           = "pop"
                                ),
                                selected = "lifeExp"),
                    sliderInput("di_anio", "Año de Análisis:",
                                min = 1952, max = 2007, value = 2007,
                                step = 5, sep = "", ticks = FALSE),
                    checkboxInput("di_log", "Escala logarítmica en X", value = FALSE),
                    hr(),
                    tags$h5(style="color:#aaa;","📐 Estadísticas Descriptivas"),
                    verbatimTextOutput("di_stats")
                ),
                box(title = "Histograma de Frecuencias",
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("di_histograma", height = "350px"))
              ),
              fluidRow(
                box(title = "Boxplot por Continente",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("di_boxplot", height = "380px"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 5 — CORRELACIONES
      # ════════════════════════════════════════════════════
      tabItem(tabName = "correlations",
              fluidRow(
                box(title = "⚙️ Controles", status = "warning",
                    solidHeader = TRUE, width = 3,
                    sliderInput("co_anio", "Año de Análisis:",
                                min = 1952, max = 2007, value = 2007,
                                step = 5, sep = "", ticks = FALSE),
                    checkboxInput("co_log_x",    "Escala log en eje X",       value = TRUE),
                    checkboxInput("co_tendencia","Mostrar línea de tendencia", value = TRUE),
                    hr(),
                    tags$small(style="color:#999;",
                               "💡 Activa 'escala log' para ver mejor las diferencias entre países pobres y ricos.")
                ),
                box(title = "PIB per Cápita vs Expectativa de Vida",
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("co_dispersion1", height = "380px"))
              ),
              fluidRow(
                box(title = "Población vs Expectativa de Vida",
                    status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("co_dispersion2", height = "340px")),
                box(title = "Mapa de Calor: Correlaciones entre Variables",
                    status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("co_heatmap", height = "340px"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 6 — MODELO ESTADÍSTICO
      # ════════════════════════════════════════════════════
      tabItem(tabName = "model",
              fluidRow(
                box(title = "⚙️ Configuración del Modelo",
                    status = "warning", solidHeader = TRUE, width = 3,
                    selectInput("mo_continente", "Continente:",
                                choices = c("Todos" = "All", levels(gapminder$continent)),
                                selected = "All"),
                    selectInput("mo_anio", "Año:",
                                choices  = sort(unique(gapminder$year)),
                                selected = 2007),
                    selectInput("mo_x", "Variable Independiente (X):",
                                choices = c(
                                  "PIB per Cápita" = "gdpPercap",
                                  "Población"      = "pop"
                                ),
                                selected = "gdpPercap"),
                    checkboxInput("mo_log_x", "Aplicar Log a la variable X", value = TRUE),
                    hr(),
                    tags$h5(style="color:#aaa;","📐 Métricas del Modelo"),
                    tableOutput("mo_metricas")
                ),
                box(title = "Regresión Lineal: Expectativa de Vida ~ Variable Seleccionada",
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("mo_grafico", height = "450px"))
              ),
              fluidRow(
                box(title = "📋 Resumen Estadístico Completo (R output)",
                    status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("mo_resumen")),
                box(title = "💡 Interpretación Automática",
                    status = "success", solidHeader = TRUE, width = 6,
                    uiOutput("mo_interpretacion"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 7 — DATOS COMPLETOS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "data",
              fluidRow(
                box(title = "🗃️ Base de Datos Gapminder — Filtrable y Descargable",
                    status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("dt_tabla_completa"))
              )
      ),
      # =========================================================================
      # TAB 8 — RANKING Y MAPA MUNDIAL
      # =========================================================================
      tabItem(tabName = "ranking_mapa",
              fluidRow(
                # Caja para el Gráfico de Ranking (Barras horizontales interactivo)
                box(title = "🏆 Top Países por Esperanza de Vida",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 4,
                    plotlyOutput("plot_ranking", height = "450px")),
                
                # Caja para el Mapa Coroplético o Interactivo
                box(title = "🗺️ Distribución Geográfica Global Esperanza de Vida",
                    status = "success",
                    solidHeader = TRUE,
                    width = 8,
                    leafletOutput("mapa_esperanza", height = "650px"))
              )
      )
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# ============================================================
#  SERVIDOR (SERVER)
# ============================================================
server <- function(input, output, session) {
  
  # ── Datos reactivos filtrados por filtros globales ────────
  datos_filtrados <- reactive({
    df <- gapminder
    if (input$g_continent != "All")
      df <- df %>% filter(continent == input$g_continent)
    df %>% filter(year >= input$g_year[1], year <= input$g_year[2])
  })
  
  # Datos del último año disponible dentro del filtro
  datos_ultimo <- reactive({
    datos_filtrados() %>% filter(year == max(year))
  })
  
  # ── TAB 1: KPIs ───────────────────────────────────────────
  output$kpi_paises <- renderValueBox({
    valueBox(
      n_distinct(datos_ultimo()$country),
      "Países analizados",
      icon  = icon("flag"),
      color = "blue"
    )
  })
  
  output$kpi_vida <- renderValueBox({
    val <- round(mean(datos_ultimo()$lifeExp, na.rm = TRUE), 1)
    valueBox(
      paste0(val, " años"),
      "Expectativa de Vida Promedio",
      icon  = icon("heart"),
      color = "green"
    )
  })
  
  output$kpi_gdp <- renderValueBox({
    val <- round(mean(datos_ultimo()$gdpPercap, na.rm = TRUE), 0)
    valueBox(
      paste0("$", format(val, big.mark = ",")),
      "PIB per Cápita Promedio",
      icon  = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$kpi_poblacion <- renderValueBox({
    val <- sum(datos_ultimo()$pop, na.rm = TRUE)
    valueBox(
      paste0(round(val / 1e9, 2), " B"),
      "Población Total",
      icon  = icon("users"),
      color = "red"
    )
  })
  
  # ── TAB 1: Gráficos de overview ───────────────────────────
  output$ov_vida_cont <- renderPlotly({
    df <- datos_ultimo() %>%
      group_by(continent) %>%
      summarise(promedio = mean(lifeExp, na.rm = TRUE), .groups = "drop") %>%
      arrange(promedio)
    
    plot_ly(df,
            x = ~promedio,
            y = ~reorder(continent, promedio),
            type = "bar", orientation = "h",
            marker = list(color = continent_colors[as.character(df$continent)]),
            text  = ~round(promedio, 1),
            textposition = "outside") %>%
      dark_theme() %>%
      layout(xaxis = list(title = "Años"),
             yaxis = list(title = ""))
  })
  
  output$ov_gdp_cont <- renderPlotly({
    df <- datos_ultimo() %>%
      group_by(continent) %>%
      summarise(promedio = mean(gdpPercap, na.rm = TRUE), .groups = "drop") %>%
      arrange(promedio)
    
    plot_ly(df,
            x = ~promedio,
            y = ~reorder(continent, promedio),
            type = "bar", orientation = "h",
            marker = list(color = continent_colors[as.character(df$continent)]),
            text  = ~paste0("$", format(round(promedio), big.mark = ",")),
            textposition = "outside") %>%
      dark_theme() %>%
      layout(xaxis = list(title = "USD"),
             yaxis = list(title = ""))
  })
  
  output$ov_tendencia <- renderPlotly({
    df <- datos_filtrados() %>%
      group_by(year, continent) %>%
      summarise(vida_prom = mean(lifeExp, na.rm = TRUE), .groups = "drop")
    
    plot_ly(df,
            x = ~year, y = ~vida_prom, color = ~continent,
            colors = continent_colors,
            type = "scatter", mode = "lines+markers",
            line   = list(width = 2),
            marker = list(size  = 5),
            text   = ~paste0(continent, " (", year, "): ",
                             round(vida_prom, 1), " años"),
            hoverinfo = "text") %>%
      dark_theme() %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = "Expectativa de Vida Promedio (años)"))
  })
  
  # ── TAB 2: Tendencias ─────────────────────────────────────
  output$tr_grafico <- renderPlotly({
    req(input$tr_paises)
    df  <- gapminder %>%
      filter(as.character(country) %in% input$tr_paises)
    var <- input$tr_variable
    
    plot_ly(df,
            x = ~year, y = ~get(var), color = ~country,
            type = "scatter", mode = "lines+markers",
            line   = list(width = 2),
            marker = list(size = 6),
            text   = ~paste0(country, "<br>Año: ", year,
                             "<br>", var_label(var), ": ", round(get(var), 1)),
            hoverinfo = "text") %>%
      dark_theme() %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = var_label(var)))
  })
  
  output$tr_tabla <- renderDT({
    req(input$tr_paises)
    df <- gapminder %>%
      filter(as.character(country) %in% input$tr_paises) %>%
      mutate(gdpPercap = round(gdpPercap, 1),
             lifeExp   = round(lifeExp, 1)) %>%
      rename(País        = country,
             Continente  = continent,
             Año         = year,
             `Exp. Vida (años)` = lifeExp,
             `PIB/cápita (USD)` = gdpPercap,
             Población          = pop)
    datatable(df,
              options = list(
                pageLength = 10, scrollX = TRUE,
                dom = "Bfrtip", buttons = c("csv", "excel")
              ),
              extensions = "Buttons"
    )
  })
  
  # ── TAB 3: Burbujas Animadas ──────────────────────────────
  output$bb_burbujas <- renderPlotly({
    df <- gapminder %>%
      mutate(etiqueta = paste0(
        "<b>", country, "</b>",
        "<br>Año: ",      year,
        "<br>PIB/cáp: $", format(round(gdpPercap), big.mark = ","),
        "<br>Exp.Vida: ",  round(lifeExp, 1), " años",
        "<br>Población: ", format(pop, big.mark = ",")
      ))
    
    plot_ly(df,
            x = ~gdpPercap, y = ~lifeExp,
            size = ~pop, color = ~continent,
            colors = continent_colors,
            frame = ~year,
            text  = ~etiqueta, hoverinfo = "text",
            type = "scatter", mode = "markers",
            sizes = c(5, 80),
            marker = list(
              opacity  = 0.75,
              sizemode = "diameter",
              line     = list(width = 0.5, color = "white")
            )) %>%
      dark_theme() %>%
      layout(
        xaxis = list(type = "log",
                     title = "PIB per Cápita (escala logarítmica, USD)"),
        yaxis = list(title = "Expectativa de Vida (años)",
                     range = c(20, 90))
      ) %>%
      animation_opts(800, easing = "elastic", redraw = FALSE) %>%
      animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom",
        label = "▶  Play"
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Año en pantalla: ",
          font   = list(color = "#3498db", size = 15)
        )
      )
  })
  
  # ── TAB 4: Distribuciones ────────────────────────────────
  output$di_stats <- renderPrint({
    df <- gapminder %>%
      filter(year == input$di_anio) %>%
      pull(input$di_variable)
    
    cat("Año seleccionado:", input$di_anio, "\n")
    cat("N (países):", length(df),         "\n")
    cat("Media:  ",   round(mean(df),   2), "\n")
    cat("Mediana:",   round(median(df), 2), "\n")
    cat("Desv.Est.:", round(sd(df),     2), "\n")
    cat("Mínimo: ",   round(min(df),    2), "\n")
    cat("Máximo: ",   round(max(df),    2), "\n")
  })
  
  output$di_histograma <- renderPlotly({
    df   <- gapminder %>% filter(year == input$di_anio)
    var  <- input$di_variable
    xval <- if (input$di_log) log10(df[[var]] + 1) else df[[var]]
    
    plot_ly(x = xval, type = "histogram", nbinsx = 25,
            marker = list(color  = "#3498db", opacity = 0.85,
                          line   = list(width = 0.5, color = "white"))) %>%
      dark_theme() %>%
      layout(
        xaxis = list(title = ifelse(input$di_log,
                                    paste("log10(", var_label(var), ")"),
                                    var_label(var))),
        yaxis = list(title = "Frecuencia (nº de países)")
      )
  })
  
  output$di_boxplot <- renderPlotly({
    df  <- gapminder %>% filter(year == input$di_anio)
    var <- input$di_variable
    
    plot_ly(df,
            x = ~continent, y = ~get(var),
            color = ~continent, colors = continent_colors,
            type = "box", boxpoints = "outliers",
            text      = ~paste0(country, ": ", round(get(var), 1)),
            hoverinfo = "text") %>%
      dark_theme() %>%
      layout(
        xaxis      = list(title = "Continente"),
        yaxis      = list(title = var_label(var)),
        showlegend = FALSE
      )
  })
  
  # ── TAB 5: Correlaciones ─────────────────────────────────
  output$co_dispersion1 <- renderPlotly({
    df <- gapminder %>% filter(year == input$co_anio)
    
    p <- plot_ly(df,
                 x = ~gdpPercap, y = ~lifeExp,
                 color = ~continent, colors = continent_colors,
                 type = "scatter", mode = "markers",
                 text = ~paste0(country,
                                "<br>PIB: $", format(round(gdpPercap), big.mark = ","),
                                "<br>Exp. Vida: ", round(lifeExp, 1), " años"),
                 hoverinfo = "text",
                 marker = list(size = 9, opacity = 0.85))
    
    if (input$co_log_x)
      p <- p %>% layout(xaxis = list(type = "log"))
    
    p %>%
      dark_theme() %>%
      layout(xaxis = list(title = "PIB per Cápita (USD)"),
             yaxis = list(title = "Expectativa de Vida (años)"))
  })
  
  output$co_dispersion2 <- renderPlotly({
    df <- gapminder %>% filter(year == input$co_anio)
    
    plot_ly(df,
            x = ~pop, y = ~lifeExp,
            color = ~continent, colors = continent_colors,
            type = "scatter", mode = "markers",
            text = ~paste0(country,
                           "<br>Pob: ", format(pop, big.mark = ","),
                           "<br>Exp. Vida: ", round(lifeExp, 1), " años"),
            hoverinfo = "text",
            marker = list(size = 9, opacity = 0.85)) %>%
      dark_theme() %>%
      layout(xaxis = list(type = "log", title = "Población (escala log)"),
             yaxis = list(title = "Expectativa de Vida (años)"))
  })
  
  output$co_heatmap <- renderPlotly({
    df    <- gapminder %>% select(lifeExp, gdpPercap, pop, year)
    corr  <- round(cor(df, use = "complete.obs"), 3)
    etiq  <- c("Exp. Vida", "PIB/cáp.", "Población", "Año")
    colnames(corr) <- rownames(corr) <- etiq
    n     <- nrow(corr)
    
    plot_ly(
      x = etiq, y = rev(etiq),
      z = corr[rev(seq_len(n)), ],
      type = "heatmap", colorscale = "RdBu",
      zmin = -1, zmax = 1,
      text = corr[rev(seq_len(n)), ],
      texttemplate = "%{text}"
    ) %>%
      dark_theme() %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  # ── TAB 6: Modelo Estadístico ────────────────────────────
  # Datos para el modelo
  datos_modelo <- reactive({
    df <- gapminder %>% filter(year == as.integer(input$mo_anio))
    if (input$mo_continente != "All")
      df <- df %>% filter(continent == input$mo_continente)
    df
  })
  
  # Ajuste del modelo lineal
  modelo_ajuste <- reactive({
    df   <- datos_modelo()
    xvar <- input$mo_x
    df$x_trans <- if (input$mo_log_x) log(df[[xvar]]) else df[[xvar]]
    lm(lifeExp ~ x_trans, data = df)
  })
  
  output$mo_grafico <- renderPlotly({
    df   <- datos_modelo()
    xvar <- input$mo_x
    fit  <- modelo_ajuste()
    df$ajustado <- fitted(fit)
    df$x_plot   <- df[[xvar]]
    
    p <- plot_ly() %>%
      add_markers(
        data      = df,
        x         = ~x_plot, y = ~lifeExp, color = ~continent,
        colors    = continent_colors,
        text      = ~paste0(country,
                            "<br>", var_label(xvar), ": ", round(x_plot, 1),
                            "<br>Exp. Vida: ", round(lifeExp, 1), " años"),
        hoverinfo = "text",
        marker    = list(size = 9, opacity = 0.8)
      ) %>%
      add_lines(
        data = df %>% arrange(x_plot),
        x    = ~x_plot, y = ~ajustado,
        line = list(color = "white", width = 2, dash = "dash"),
        name = "Línea de Regresión", showlegend = TRUE
      )
    
    x_type <- if (input$mo_log_x) "log" else "linear"
    p %>%
      dark_theme() %>%
      layout(
        xaxis = list(type = x_type, title = var_label(xvar)),
        yaxis = list(title = "Expectativa de Vida (años)")
      )
  })
  
  output$mo_resumen <- renderPrint({
    summary(modelo_ajuste())
  })
  
  output$mo_metricas <- renderTable({
    s  <- summary(modelo_ajuste())
    pv <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
             lower.tail = FALSE)
    data.frame(
      Métrica = c("R²", "R² Ajustado", "Error Residual",
                  "F-estadístico", "p-valor"),
      Valor   = c(
        round(s$r.squared,     4),
        round(s$adj.r.squared, 4),
        round(s$sigma,         4),
        round(s$fstatistic[1], 4),
        format.pval(pv, digits = 3)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "l")
  
  output$mo_interpretacion <- renderUI({
    s   <- summary(modelo_ajuste())
    r2  <- round(s$r.squared * 100, 1)
    b   <- round(coef(modelo_ajuste()), 3)
    pv  <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
              lower.tail = FALSE)
    sig <- ifelse(pv < 0.05,
                  "<span style='color:#2ecc71;'>✅ El modelo es estadísticamente significativo (p &lt; 0.05)</span>",
                  "<span style='color:#e74c3c;'>⚠️ El modelo NO es significativo (p ≥ 0.05)</span>"
    )
    poder <- if (r2 >= 70) "ALTO ✅" else if (r2 >= 40) "MODERADO ⚠️" else "BAJO ❌"
    
    HTML(paste0("
      <div style='padding:15px; color:#ccc; line-height:2;'>
        <h4 style='color:#3498db; margin-top:0;'>📊 Resultados del Modelo</h4>
        <p><b>Intercepto (β₀) = ", b[1], "</b><br>
           <small>Valor esperado de la expectativa de vida cuando log(X) = 0</small></p>
        <p><b>Pendiente (β₁) = ", b[2], "</b><br>
           <small>Cambio en expectativa de vida por cada unidad adicional en log(X)</small></p>
        <hr style='border-color:#333;'>
        <p><b>R² = ", r2, "%</b> → Poder explicativo: <b>", poder, "</b><br>
           <small>El modelo explica el ", r2, "% de la variabilidad en expectativa de vida.</small></p>
        <p>", sig, "</p>
        <hr style='border-color:#333;'>
        <p><small style='color:#999;'>
          Un R² alto indica que la variable seleccionada es un buen predictor de la expectativa de vida.<br>
          Un p-valor &lt; 0.05 confirma que la relación no se debe al azar.
        </small></p>
      </div>
    "))
  })
  
  # ── TAB 7: Tabla completa ────────────────────────────────
  output$dt_tabla_completa <- renderDT({
    df <- datos_filtrados() %>%
      mutate(
        gdpPercap = round(gdpPercap, 1),
        lifeExp   = round(lifeExp,   1)
      ) %>%
      rename(
        País                 = country,
        Continente           = continent,
        Año                  = year,
        `Exp. Vida (años)`   = lifeExp,
        `PIB/cápita (USD)`   = gdpPercap,
        Población            = pop
      )
    
    datatable(df,
              filter     = "top",
              extensions = "Buttons",
              options    = list(
                pageLength = 15,
                scrollX    = TRUE,
                dom        = "Bfrtip",
                buttons    = list("copy", "csv", "excel", "pdf")
              )
    ) %>%
      formatCurrency("PIB/cápita (USD)", currency = "$", digits = 1) %>%
      formatStyle(
        "Exp. Vida (años)",
        background         = styleColorBar(range(gapminder$lifeExp), "#2ecc71"),
        backgroundSize     = "100% 88%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  
  # =========================================================================
  # # TAB 8: Ranking y Mapa
  # =========================================================================
  
  # 1. Renderizado del Gráfico de Ranking (Top Países) ----
  output$plot_ranking <- renderPlotly({
    
    # Procesamos los datos usando tu reactivo 'datos_filtrados()'
    df_ranking <- datos_filtrados() %>%
      # Agrupamos por país y calculamos la esperanza de vida promedio
      group_by(country) %>%
      summarise(LifeExp_mean = mean(lifeExp, na.rm = TRUE), .groups = 'drop') %>%
      # Traducimos los nombres de países al español
      mutate(country_es = ifelse(
        country %in% names(paises_es),
        paises_es[as.character(country)],
        as.character(country)
      )) %>%
      # Ordenamos de mayor a menor esperanza de vida y tomamos los 15 mejores
      arrange(desc(LifeExp_mean)) %>%
      head(15)
    
    # Construcción del gráfico con ggplot simulando el estilo limpio de tu app
    p <- ggplot(df_ranking, aes(x = reorder(country_es, LifeExp_mean), y = LifeExp_mean, fill = LifeExp_mean)) +
      geom_col(width = 0.75) +
      coord_flip() + # Barras horizontales para mejor lectura de nombres
      scale_fill_gradient(low = "#a1dbcd", high = "#2ecc71") + # Usando tu color verde '#2ecc71'
      labs(
        x = NULL,
        y = "Esperanza de Vida (Años)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold")
      )
    
    # Convertimos a formato interactivo con Plotly
    ggplotly(p, tooltip = c("y")) %>% 
      config(displayModeBar = FALSE) # Oculta la barra de herramientas molesta de plotly
  })
  
  
  # 2. Renderizado del Mapa con Leaflet ----
  output$mapa_esperanza <- renderLeaflet({
    
    # Preparar datos geográficos rápidos
    df_mapa <- datos_filtrados() %>%
      group_by(country) %>%
      summarise(LifeExp_mean = round(mean(lifeExp, na.rm = TRUE), 1), .groups = 'drop') %>%
      mutate(country_es = ifelse(
        country %in% names(paises_es),
        paises_es[as.character(country)],
        as.character(country)
      ))
    
    # NOTA: Leaflet nativo requiere coordenadas (Lat/Long). Si tu dataset 'gapminder' 
    # original no tiene coordenadas, puedes cruzarlas rápidamente usando un dataset interno de R:
    coordenadas_mundo <- maps::world.cities %>% 
      group_by(country.etc) %>% 
      summarise(lat = mean(lat), lng = mean(long)) %>%
      rename(country = country.etc)
    
    df_mapa_geo <- df_mapa %>%
      inner_join(coordenadas_mundo, by = "country")
    
    # Definimos una paleta de colores coherente con tu barra verde '#2ecc71'
    pal <- colorNumeric(
      palette = c("#e8f8f5", "#2ecc71", "#1abc9c"),
      domain = df_mapa_geo$LifeExp_mean
    )
    
    # Construcción del mapa interactivo
    leaflet(df_mapa_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Fondo minimalista elegante
      setView(lng = 0, lat = 20, zoom = 1.5) %>% # Vista global inicial
      addCircleMarkers(
        lng = ~lng, 
        lat = ~lat,
        radius = ~LifeExp_mean / 3.5, # Tamaño proporcional a la expectativa de vida
        color = ~pal(LifeExp_mean),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.75,
        popup = ~paste0("<strong>País:</strong> ", country_es, "<br>",
                        "<strong>Exp. de Vida:</strong> ", LifeExp_mean, " años")
      ) %>%
      addLegend(
        pal = pal, 
        values = ~LifeExp_mean, 
        title = "Años de Vida",
        position = "bottomright"
      )
  })
  
} # end server

# ── Ejecutar la aplicación ──────────────────────────────────
shinyApp(ui = ui, server = server)
