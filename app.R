library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(purrr)
library(plotly)
library(ggthemes)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

data <- read_csv('data/processed_df.csv')

opt_dropdown_neighbourhood <- unique(data$Neighborhood) %>%
    map(function(col) list(label = col, value = col))
opt_dropdown_neighbourhood <- opt_dropdown_neighbourhood[-c(20, 24, 25)]

opt_radio_year <- list(list(label = '2017', value = 2017),
                       list(label = '2018', value = 2018),
                       list(label = '2019', value = 2019),
                       list(label = '2020', value = 2020),
                       list(label = '2021', value = 2021))
# summary card
card1 <- dbcCard(
    list(
        htmlH4("Total Number of Crimes", className = "card-title", style = list("marginLeft" = 50)),
        htmlDiv(id = "summary", style = list("color" = "#E33B18", "fontSize" = 25, "marginLeft" = 140))
    ),
    style = list("width" = "25rem", "marginLeft" = 20),
    body = TRUE,
    color = "light"
)

# filters card
card2 <- dbcCard(
    list(
        # Dropdown for neighbourhood
        htmlH5("Neighbourhood", className="text-dark"),
        dccDropdown(id = "neighbourhood_input",
                    options = opt_dropdown_neighbourhood, 
                    value = 'Kitsilano',
                    className="dropdown"),
        htmlBr(),
        htmlBr(),
        htmlBr(),
        # Radio button for year
        htmlH5("Year", className="text-dark"),
        dccRadioItems(id = "year_radio",
                      options = opt_radio_year, 
                      value = 2021,
                      className="radiobutton",
                      labelStyle = list("display" = "in-block", "marginLeft" = 20)),
        htmlBr(),
        htmlBr(),
        htmlBr()
    ),
    style = list("width" = "25rem", "marginLeft" = 20),
    body = TRUE,
    color = "light"
)

# filter layout
filter_panel = list(
    htmlH2("Vancouver Crime Dashboard", style = list("marginLeft" = 20)),
    htmlBr(),
    htmlBr(),
    card1,
    htmlBr(),
    htmlBr(),
    htmlH4("Filters", style = list("marginLeft" = 20)),
    card2,
    htmlBr()
)

# plots layout
plot_body = list(
    dccGraph("line_plot")
)

# Page layout
page_layout <- htmlDiv(
    className="page_layout",
    children=list(
        dbcRow(htmlBr()),
        dbcRow(
            list(dbcCol(filter_panel, className = "panel", width = 3),
                 dbcCol(plot_body, className = "body"))
        )
    )
)

# Overall layout
app$layout(htmlDiv(id="main", className="app", children=page_layout))

# functions
app$callback(
    output("summary", "children"),
    list(input("neighbourhood_input", "value"),
         input("year_radio", "value")),
    function(neighbourhood, year) {
        data_summary <- data %>%
            filter(Neighborhood == neighbourhood, YEAR == year)
        nrow(data_summary)
    }
)

app$callback(
    output("line_plot", 'figure'),
    list(input("neighbourhood_input", "value"),
         input("year_radio", "value")),
    function(neighbourhood, time){
        line_data <- data %>%
            group_by(TIME, YEAR, Neighborhood) %>%
            filter(Neighborhood == neighbourhood) %>%
            add_count(HOUR)
        
        line_chart <-  line_data %>%
            ggplot(aes(x = YEAR, y = n, color = TIME)) +
            geom_line(stat = 'summary', fun = mean) + 
            labs(title = "Crimes over time", x = "Year", y = "Number of Crimes") +
            theme(
                plot.title = element_text(face = "bold", size = 14),
                axis.title = element_text(face = "bold", size = 10)
            ) +
            scale_color_manual(values = c("red", "orange"))
        
        ggplotly(line_chart + aes(text = n), tooltip = c("Type", "n"), width = 800, height = 400)
    }
)

app$run_server(host = '0.0.0.0')
