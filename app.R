# Azores long-term lake explorer
# Reads only the processed CSVs in data/. Empty tiles = not in this project.
# Coverage/richness are static ggplot (plotly of the full heatmap timed out on shinyapps).

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(readr)

data_dir <- "data"
if (!file.exists(file.path(data_dir, "lake_lookup.csv"))) {
  data_dir <- file.path("shiny", "data")
}

island_levels <- c("Corvo", "Flores", "Pico", "Sao Miguel", "Sao Jorge")
island_labels <- c(
  Corvo = "Corvo",
  Flores = "Flores",
  Pico = "Pico",
  `Sao Miguel` = "São Miguel",
  `Sao Jorge` = "São Jorge"
)
island_cols <- c(
  Corvo = "#4C78A8",
  Flores = "#F58518",
  Pico = "#54A24B",
  `Sao Miguel` = "#E45756",
  `Sao Jorge` = "#72B7B2"
)
season_levels <- c("Winter", "Spring", "Summer", "Autumn")
season_short <- c(Winter = "W", Spring = "Sp", Summer = "Su", Autumn = "A")
group_levels <- c("Diatom", "Phyto", "Zoo", "Macro", "Env")
group_from_sample <- c(
  diatom = "Diatom",
  phyto = "Phyto",
  zoo = "Zoo",
  macro = "Macro",
  macrophyte = "Macrophyte"
)

lakes <- read_csv(file.path(data_dir, "lake_lookup.csv"), show_col_types = FALSE) %>%
  mutate(
    name_display = ifelse(is.na(name_recent) | name_recent == "", name_oikos, name_recent),
    island_oikos = factor(island_oikos, levels = island_levels)
  ) %>%
  arrange(island_oikos, name_display)

sites <- read_csv(file.path(data_dir, "lake_sites.csv"), show_col_types = FALSE) %>%
  mutate(
    island_oikos = factor(island_oikos, levels = island_levels),
    island_lab = unname(island_labels[as.character(island_oikos)]),
    popup = paste0(
      "<b>", name_display, "</b><br>",
      island_lab,
      ifelse(is.na(typology_oikos) | typology_oikos == "", "", paste0("<br>", typology_oikos)),
      ifelse(is.na(alt_m), "", paste0("<br>", alt_m, " m")),
      "<br>", match_status,
      ifelse(is.na(coord_note) | coord_note == "", "", paste0("<br><i>", coord_note, "</i>"))
    )
  )

coverage <- read_csv(
  file.path(data_dir, "coverage_heatmap_seasonal.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    group = factor(group, levels = group_levels),
    Season = factor(Season, levels = season_levels),
    island_oikos = factor(island_oikos, levels = island_levels),
    present = as.logical(present),
    status = ifelse(present, "Present", "Not in project")
  )

samples <- read_csv(
  file.path(data_dir, "richness_timeseries_samples.csv"),
  show_col_types = FALSE
) %>%
  filter(group %in% names(group_from_sample)) %>%
  mutate(
    dataset = factor(unname(group_from_sample[group]), levels = c(group_levels, "Macrophyte")),
    Season = factor(Season, levels = season_levels),
    island_oikos = factor(island_oikos, levels = island_levels)
  )

annual <- read_csv(
  file.path(data_dir, "richness_timeseries_annual.csv"),
  show_col_types = FALSE
) %>%
  filter(group %in% names(group_from_sample)) %>%
  mutate(
    dataset = factor(unname(group_from_sample[group]), levels = c(group_levels, "Macrophyte")),
    island_oikos = factor(island_oikos, levels = island_levels)
  )

lake_choices <- lakes$name_display
names(lake_choices) <- paste0(lakes$name_display, " (", island_labels[as.character(lakes$island_oikos)], ")")

year_min <- min(coverage$Year, na.rm = TRUE)
year_max <- max(coverage$Year, na.rm = TRUE)

or_all <- function(selected, all_vals) {
  if (is.null(selected) || !length(selected)) all_vals else selected
}

ui <- page_sidebar(
  title = "Azores long-term lakes",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 300,
    p("Processed monitoring tables only. Empty cells mean that season is not in this project, not that a campaign did not happen. 2018 SPR18 macros and chemistry are Spring (Mar–Apr)."),
    checkboxGroupInput(
      "islands",
      "Island",
      choices = setNames(island_levels, unname(island_labels[island_levels])),
      selected = island_levels
    ),
    selectizeInput(
      "lakes",
      "Lake (empty = all)",
      choices = lake_choices,
      selected = NULL,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "All lakes")
    ),
    checkboxGroupInput(
      "groups",
      "Dataset",
      choices = group_levels,
      selected = c("Diatom", "Phyto", "Macro")
    ),
    sliderInput(
      "years",
      "Years",
      min = year_min,
      max = year_max,
      value = c(year_min, year_max),
      step = 1,
      sep = ""
    ),
    checkboxGroupInput(
      "seasons",
      "Season",
      choices = setNames(season_levels, paste0(season_levels, " (", season_short[season_levels], ")")),
      selected = season_levels
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Sampling sites"),
    leafletOutput("sites_map", height = "560px"),
    card_footer("Lake locations (WGS84). Click a point for name and island. Rasa Serra Devassa uses the São Miguel Serra Devassa position, not the Flores Rasa coordinates copied into the Oikos environment file. Cubres Este/Oeste are approximate.")
  ),
  navset_card_underline(
    nav_panel(
      "Coverage",
      plotOutput("coverage_plot", height = "1400px"),
      p(class = "text-muted", "Each year is four tiles: W / Sp / Su / A.")
    ),
    nav_panel(
      "Richness",
      radioButtons(
        "rich_mode",
        NULL,
        choices = c("Annual mean (seasons averaged)" = "annual", "Seasonal samples" = "seasonal"),
        selected = "annual",
        inline = TRUE
      ),
      plotOutput("richness_plot", height = "1100px"),
      p(class = "text-muted", "Env has no taxon richness. Zooplankton is sparse (2011–2012 and spring 2025).")
    ),
    nav_panel(
      "Table",
      DTOutput("sample_table")
    )
  )
)

server <- function(input, output, session) {
  islands_f <- reactive(or_all(input$islands, island_levels))
  lakes_f <- reactive({
    keep <- lakes %>% filter(as.character(island_oikos) %in% islands_f())
    or_all(input$lakes, keep$name_display)
  })
  groups_f <- reactive(or_all(input$groups, group_levels))
  seasons_f <- reactive(or_all(input$seasons, season_levels))
  years_f <- reactive({
    if (is.null(input$years) || length(input$years) < 2) c(year_min, year_max) else input$years
  })

  observeEvent(input$islands, {
    keep <- lakes %>% filter(as.character(island_oikos) %in% islands_f())
    choices <- keep$name_display
    names(choices) <- paste0(keep$name_display, " (", island_labels[as.character(keep$island_oikos)], ")")
    still <- intersect(input$lakes, keep$name_display)
    updateSelectizeInput(session, "lakes", choices = choices, selected = still)
  }, ignoreInit = TRUE)

  sites_f <- reactive({
    sites %>%
      filter(
        as.character(island_oikos) %in% islands_f(),
        name_display %in% lakes_f()
      )
  })

  cov_f <- reactive({
    yr <- years_f()
    coverage %>%
      filter(
        as.character(island_oikos) %in% islands_f(),
        name_display %in% lakes_f(),
        as.character(group) %in% groups_f(),
        Year >= yr[1],
        Year <= yr[2],
        as.character(Season) %in% seasons_f()
      )
  })

  output$sites_map <- renderLeaflet({
    d <- sites_f()
    pal <- colorFactor(island_cols, domain = island_levels, na.color = "#888888")
    m <- leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_wgs84,
        lat = ~lat_wgs84,
        radius = 7,
        stroke = TRUE,
        color = "#222222",
        weight = 1,
        fillColor = ~pal(as.character(island_oikos)),
        fillOpacity = 0.9,
        label = ~name_display,
        popup = ~popup
      ) %>%
      addLegend(
        position = "bottomleft",
        colors = unname(island_cols[as.character(sort(unique(d$island_oikos)))]),
        labels = unname(island_labels[as.character(sort(unique(d$island_oikos)))]),
        title = "Island"
      )
    if (nrow(d)) {
      m <- m %>% fitBounds(
        lng1 = min(d$lon_wgs84) - 0.15,
        lat1 = min(d$lat_wgs84) - 0.12,
        lng2 = max(d$lon_wgs84) + 0.15,
        lat2 = max(d$lat_wgs84) + 0.12
      )
    }
    m
  })

  output$coverage_plot <- renderPlot({
    d <- cov_f()
    validate(need(nrow(d) > 0, "No rows match the filters."))
    lake_ord <- lakes %>%
      filter(name_display %in% unique(d$name_display)) %>%
      arrange(island_oikos, name_display) %>%
      pull(name_display) %>%
      unique()
    d <- d %>%
      mutate(
        name_display = factor(name_display, levels = rev(lake_ord)),
        x_ord = Year + (as.integer(Season) - 1) / 4
      )
    ggplot(d, aes(x_ord, name_display, fill = status)) +
      geom_tile(width = 0.22, height = 0.9, colour = "white", linewidth = 0.15) +
      scale_fill_manual(
        values = c(Present = "#2C6E9B", `Not in project` = "#E6E6E6"),
        breaks = c("Present", "Not in project")
      ) +
      scale_x_continuous(
        breaks = sort(unique(d$Year)),
        labels = sort(unique(d$Year))
      ) +
      facet_grid(group ~ ., scales = "free_y", space = "free_y") +
      labs(x = "Year (each year is W / Sp / Su / A)", y = NULL, fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom"
      )
  })

  output$richness_plot <- renderPlot({
    bio_groups <- setdiff(groups_f(), "Env")
    validate(need(length(bio_groups) > 0, "Select a biological dataset (Env has no richness)."))
    yr <- years_f()
    if (identical(input$rich_mode, "annual")) {
      d <- annual %>%
        filter(
          as.character(island_oikos) %in% islands_f(),
          name_display %in% lakes_f(),
          as.character(dataset) %in% bio_groups,
          Year >= yr[1],
          Year <= yr[2]
        )
      validate(need(nrow(d) > 0, "No richness rows match the filters."))
      ggplot(d, aes(Year, n_taxa_mean, colour = dataset, group = interaction(name_display, dataset))) +
        geom_line(alpha = 0.7) +
        geom_point(size = 1.4) +
        facet_wrap(~name_display, scales = "free_y") +
        labs(x = "Year", y = "Mean taxa per season", colour = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
    } else {
      d <- samples %>%
        filter(
          as.character(island_oikos) %in% islands_f(),
          name_display %in% lakes_f(),
          as.character(dataset) %in% bio_groups,
          Year >= yr[1],
          Year <= yr[2],
          as.character(Season) %in% seasons_f()
        )
      validate(need(nrow(d) > 0, "No richness rows match the filters."))
      ggplot(d, aes(Year, n_taxa, colour = dataset, shape = Season)) +
        geom_point(size = 1.8, alpha = 0.85) +
        facet_wrap(~name_display, scales = "free_y") +
        labs(x = "Year", y = "Taxa per sample", colour = NULL, shape = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
    }
  })

  output$sample_table <- renderDT({
    bio_groups <- setdiff(groups_f(), "Env")
    yr <- years_f()
    d <- samples %>%
      filter(
        as.character(island_oikos) %in% islands_f(),
        name_display %in% lakes_f(),
        Year >= yr[1],
        Year <= yr[2],
        as.character(Season) %in% seasons_f()
      )
    if (length(bio_groups)) {
      d <- d %>% filter(as.character(dataset) %in% bio_groups)
    }
    d %>%
      transmute(
        Lake = name_display,
        Island = unname(island_labels[as.character(island_oikos)]),
        Year,
        Season,
        Dataset = dataset,
        n_taxa,
        period
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE)
      )
  })
}

shinyApp(ui, server)
