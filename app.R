library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)   # for percent formatting
library(glue)


# Import full tennis dataset
tennis_temp <- 
  read.csv("/Users/teddykelly/Downloads/atp_matches_till_2022.csv")

# Filter the dataset to only contain the matches with match statistics
tennis_df <- tennis_temp |> dplyr::filter(!is.na(w_df))


# Delete any variable columns that are not important for the regressions

df <- tennis_df |> dplyr::select(- winner_seed, -winner_ht, 
                                 -loser_seed, -loser_ht, -minutes,
                                 -winner_ioc, -winner_entry,
                                 -loser_ioc, loser_entry
)

# Remove any incorrect observations (numerical variables with negative values)
quantitative_cols <- sapply(df, is.numeric) | sapply(df, is.integer)
quantitative_var_names <- names(df)[quantitative_cols]
df <- df |> filter(across(all_of(quantitative_var_names), ~ . >= 0))

# Remove any matches that ended in a retirement
df <- df |> filter(!grepl("RET", score))

# Remove any matches that were played on Carpet Courts
df <- df |> filter(surface != "Carpet")



# --------------- Creating new Variables -----------------


# First serve percentage winners and losers
df$w_1stInPct <- df$w_1stIn / df$w_svpt
df$l_1stInPct <- df$l_1stIn / df$l_svpt

# First serve percentage won winners and losers
df$w_1stWonPct <- df$w_1stWon / df$w_1stIn
df$l_1stWonPct <- df$l_1stWon / df$l_1stIn

# 2nd serve % won winners and losers
df$w_2ndWonPct <- df$w_2ndWon / (df$w_svpt - df$w_1stIn)
df$l_2ndWonPct <- df$l_2ndWon / (df$l_svpt - df$l_1stIn)

# Break point save percentage
df$w_bpSavedPct <- df$w_bpSaved / df$w_bpFaced
df$l_bpSavedPct <- df$l_bpSaved / df$l_bpFaced
df$w_bpSavedPct[is.na(df$w_bpSavedPct)] <- 1
df$l_bpSavedPct[is.na(df$l_bpSavedPct)] <- 1

# Break points converted
df$w_bpConv <- df$l_bpFaced - df$l_bpSaved
df$l_bpConv <- df$w_bpFaced - df$w_bpSaved


# Break Point conversion rate winners and losers
df$w_bpConvPct <- (1 - df$l_bpSavedPct)
df$l_bpConvPct <- (1 - df$w_bpSavedPct)

# Create return points won
df$w_returnPtsWon <- (df$l_svpt - (df$l_1stWon + df$l_2ndWon))
df$l_returnPtsWon <- (df$w_svpt - (df$w_1stWon + df$w_2ndWon))

# Create return points won percentage
df$w_returnPtsWonPct <- df$w_returnPtsWon / df$l_svpt
df$l_returnPtsWonPct <- df$l_returnPtsWon / df$w_svpt


# Remove any matches with missing values
df <- na.omit(df)

# Filter dataset to only contain 2nd serve win % less than or equal to 1
df <- df|> dplyr::filter(w_2ndWonPct <= 1)



# ----------- Create Seperate DataFrames for each Surface -----------

df_clay <- df |> dplyr::filter(surface == "Clay")
df_grass <- df |> dplyr::filter(surface == "Grass")
df_hard <- df |> dplyr::filter(surface == "Hard")


# --------- Split into training and testing data for each surface ---------

# CLAY TRAINING and TESTING DATA
set.seed(123)
# Generate a vector of indices for the training set

train_index_clay <- 
  sample(x = nrow(df_clay),  size = round(0.8 * nrow(df_clay)))

train_clay <- df_clay[train_index_clay, ]

test_clay  <- df_clay[-train_index_clay, ]

# GRASS TRAINING and TESTING DATA
train_index_grass <- 
  sample(x = nrow(df_grass),  size = round(0.8 * nrow(df_grass)))

train_grass <- df_grass[train_index_grass, ]

test_grass <- df_grass[-train_index_grass, ]


# HARD TRAINING and TESTING DATA
train_index_hard <- 
  sample(x = nrow(df_hard),  size = round(0.8 * nrow(df_hard)))

train_hard <- df_hard[train_index_hard, ]

test_hard <- df_hard[-train_index_hard, ]




# ----------------------
# Helper: discover stats
# ----------------------
# We expect columns like w_1stWonPct, l_1stWonPct, w_ace, l_ace, etc.
# This app will look for pairs of columns that share the same suffix after removing the "w_" or "l_".
all_cols <- unique(c(names(train_clay), names(train_grass), names(train_hard)))

w_cols <- all_cols[grepl("^w_", all_cols)]
l_cols <- all_cols[grepl("^l_", all_cols)]

# base stat names where both w_ and l_ exist
base_names <- intersect(sub("^w_", "", w_cols), sub("^l_", "", l_cols))
base_names <- sort(base_names)

# Create human-friendly labels: e.g. "1stWonPct" -> "1st Serve Points Won %"
make_label <- function(x){
  lab <- x
  # small heuristics for nicer labels
  lab <- gsub("Pct$", "%", lab)
  lab <- gsub("Won", " Won", lab)
  lab <- gsub("1st", "1st", lab)
  lab <- gsub("2nd", "2nd", lab)
  lab <- gsub("SvGms", "Service Games", lab)
  lab <- gsub("_", " ", lab)
  lab
}
stat_choices <- setNames(base_names, sapply(base_names, make_label))

# Surfaces
surface_choices <- c("Clay", "Grass", "Hard")

ui <- fluidPage(
  titlePanel("Compare Winner vs Loser — Any Statistic by Surface"),
  sidebarLayout(
    sidebarPanel(
      selectInput("surface", "Select Surface(s):",
                  choices = surface_choices,
                  multiple = TRUE,
                  selected = surface_choices),
      selectInput("stat", "Select Statistic (Winner vs Loser):",
                  choices = stat_choices,
                  selected = base_names[which(base_names == "1stWonPct") %||% 1]),
      actionButton("run", "Update", class = "btn-primary"),
      hr(),
      helpText("")
    ),
    mainPanel(
      plotOutput("barplot", height = "480px"),
      br(),
      tableOutput("means_table")
    )
  )
)

server <- function(input, output, session) {
  
  # reactive dataset builder for selected surfaces
  get_combined <- reactive({
    req(input$surface)
    frames <- list()
    if ("Clay" %in% input$surface) frames <- c(frames, list(train_clay))
    if ("Grass" %in% input$surface) frames <- c(frames, list(train_grass))
    if ("Hard"  %in% input$surface) frames <- c(frames, list(train_hard))
    combined <- bind_rows(frames, .id = "src")  # src not used further
    combined
  })
  
  observeEvent(input$run, {
    
    df_all <- get_combined()
    req(nrow(df_all) > 0)
    
    stat_base <- input$stat
    w_var <- paste0("w_", stat_base)
    l_var <- paste0("l_", stat_base)
    
    # Safely handle missing columns
    if (!all(c(w_var, l_var) %in% names(df_all))) {
      showNotification(glue("Selected stat columns not found: {w_var} / {l_var}"), type = "error")
      return()
    }
    
    # Create tidy long dataset with player_type and value
    plot_df <- df_all %>%
      dplyr::select(surface, all_of(c(w_var, l_var))) %>%
      tidyr::pivot_longer(cols = c(all_of(w_var), all_of(l_var)),
                          names_to = "player_type", values_to = "value") %>%
      dplyr::mutate(player_type = ifelse(grepl("^w_", player_type), "Winner", "Loser"),
                    surface = as.factor(surface))
    
    # compute means and SE for errorbars and label placement
    summary_df <- plot_df %>%
      group_by(surface, player_type) %>%
      summarise(mean = mean(value, na.rm = TRUE),
                se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
                n = sum(!is.na(value)),
                .groups = "drop")
    
    # detect if stat looks like a proportion (Pct or values between 0 and 1)
    looks_pct <- grepl("Pct$|pct|Pct", stat_base) ||
      (all(plot_df$value >= 0, na.rm = TRUE) && all(plot_df$value <= 1, na.rm = TRUE))
    
    # prepare label formatting
    label_fun <- if (looks_pct) function(x) paste0(round(x * 100, 1), "%") else function(x) format(round(x, 2), nsmall = 2)
    
    # Plot
    p <- ggplot(plot_df, aes(x = surface, y = value, fill = player_type)) +
      stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.8)) +
      stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.8), width = 0.2) +
      geom_text(data = summary_df,
                aes(x = surface, y = mean, label = label_fun(mean), group = player_type),
                position = position_dodge(width = 0.8),
                vjust = -0.6, size = 4) +
      labs(title = paste0(make_label(stat_base), " — Winner vs Loser by Surface"),
           x = "Surface",
           y = make_label(stat_base),
           fill = "") +
      theme_minimal(base_size = 14)
    
    if (looks_pct) {
      p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA))
    }
    
    output$barplot <- renderPlot(p)
    
    # output table of means
    output$means_table <- renderTable({
      summary_df %>%
        tidyr::pivot_wider(names_from = player_type, values_from = c(mean, se, n)) %>%
        # format numbers nicely
        mutate(across(starts_with("mean"), ~ if (looks_pct) paste0(round(. * 100, 2), "%") else round(., 3)))
    }, striped = TRUE, hover = TRUE)
    
  }, ignoreNULL = FALSE)
  
}

shinyApp(ui, server)
