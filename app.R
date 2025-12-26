library(shiny)
library(dplyr)
library(stargazer)


library(tidyverse)
library(stargazer)
library(DataExplorer)

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

# Check for any missing values
plot_missing(df, missing_only = T)

# Remove any matches with missing values
df <- na.omit(df)

# Filter dataset to only contain 2nd serve win % less than or equal to 1
df <- df|> dplyr::filter(w_2ndWonPct <= 1)


# ----------------- Randomly Assigning A and B -----------------------
# Add column that indicates whether player A or player B won

set.seed(123)
df$swap <- rbinom(nrow(df), 1, 0.5)

# Create Win loss binary variable (1 indicates that player A won and 0 indicates player B won)
df$player_A <- ifelse(df$swap == 1, df$winner_name, df$loser_name)
df$player_B <- ifelse(df$swap == 0, df$winner_name, df$loser_name)

# Creat match_outcome variable
df$match_result <- ifelse(df$swap == 1, 1, 0)

# Rankings
df$A_rank <- ifelse(df$swap == 1, df$winner_rank, df$loser_rank)
df$B_rank <- ifelse(df$swap == 0, df$winner_rank, df$loser_rank)

df$A_rank_points <- ifelse(df$swap == 1, df$winner_rank_points, df$loser_rank_points)
df$B_rank_points <- ifelse(df$swap == 0, df$winner_rank_points, df$loser_rank_points)

# Handedness
df$A_hand <- ifelse(df$swap == 1, df$winner_hand, df$loser_hand)
df$B_hand <- ifelse(df$swap == 0, df$winner_hand, df$loser_hand)

# Aces
df$A_ace <- ifelse(df$swap == 1, df$w_ace, df$l_ace)
df$B_ace <- ifelse(df$swap == 0, df$w_ace, df$l_ace)

# Double faults
df$A_df <- ifelse(df$swap == 1, df$w_df, df$l_df)
df$B_df <- ifelse(df$swap == 0, df$w_df, df$l_df)

# Total service points
df$A_svpt <- ifelse(df$swap == 1, df$w_svpt, df$l_svpt)
df$B_svpt <- ifelse(df$swap == 0, df$w_svpt, df$l_svpt)

# First serve in
df$A_1stIn <- ifelse(df$swap == 1, df$w_1stIn, df$l_1stIn)
df$B_1stIn <- ifelse(df$swap == 0, df$w_1stIn, df$l_1stIn)

# First serve points won
df$A_1stWon <- ifelse(df$swap == 1, df$w_1stWon, df$l_1stWon)
df$B_1stWon <- ifelse(df$swap == 0, df$w_1stWon, df$l_1stWon)

# Second serve points won
df$A_2ndWon <- ifelse(df$swap == 1, df$w_2ndWon, df$l_2ndWon)
df$B_2ndWon <- ifelse(df$swap == 0, df$w_2ndWon, df$l_2ndWon)

# Service games
df$A_SvGms <- ifelse(df$swap == 1, df$w_SvGms, df$l_SvGms)
df$B_SvGms <- ifelse(df$swap == 0, df$w_SvGms, df$l_SvGms)

# Break points saved
df$A_bpSaved <- ifelse(df$swap == 1, df$w_bpSaved, df$l_bpSaved)
df$B_bpSaved <- ifelse(df$swap == 0, df$w_bpSaved, df$l_bpSaved)

# Break points faced
df$A_bpFaced <- ifelse(df$swap == 1, df$w_bpFaced, df$l_bpFaced)
df$B_bpFaced <- ifelse(df$swap == 0, df$w_bpFaced, df$l_bpFaced)

# Break Points generated on return
df$A_bpCreated <- ifelse(df$swap == 1, df$l_bpFaced, df$w_bpFaced)
df$B_bpCreated <- ifelse(df$swap == 0, df$l_bpFaced, df$w_bpFaced)

# Break Point Converted on return
df$A_bpConv <- ifelse(df$swap == 1, df$w_bpConv, df$l_bpConv)
df$B_bpConv <- ifelse(df$swap == 0, df$w_bpConv, df$l_bpConv)

# First serves made percentage
df$A_1stInPct <- df$A_1stIn / df$A_svpt
df$B_1stInPct <- df$B_1stIn / df$B_svpt

#Also going to create a first serve win % variable
df$A_1stWonPct <- df$A_1stWon / df$A_1stIn
df$B_1stWonPct <- df$B_1stWon / df$B_1stIn

# 2nd Serve Win percentage
df$A_2ndWonPct <- df$A_2ndWon / (df$A_svpt - df$A_1stIn)
df$B_2ndWonPct <- df$B_2ndWon / (df$B_svpt - df$B_1stIn)

# Break point save percentage
df$A_bpSavedPct <- df$A_bpSaved / df$A_bpFaced
df$B_bpSavedPct <- df$B_bpSaved / df$B_bpFaced
df$A_bpSavedPct[is.na(df$A_bpSavedPct)] <- 1
df$B_bpSavedPct[is.na(df$B_bpSavedPct)] <- 1

# Break Point Conversion precetage
df$A_bpConvPct <- (1 - df$B_bpSavedPct)
df$B_bpConvPct <- (1 - df$A_bpSavedPct)

# Return Points Won
df$A_returnPtsWon <- 
  ifelse(df$swap == 1, df$w_returnPtsWon, df$l_returnPtsWon)

df$B_returnPtsWon <-
  ifelse(df$swap == 0, df$w_returnPtsWon, df$l_returnPtsWon)

# Return Points Won percentage
df$A_returnPtsWonPct <-
  ifelse(df$swap == 1, df$w_returnPtsWonPct, df$l_returnPtsWonPct)
df$B_returnPtsWonPct <-
  ifelse(df$swap == 0, df$w_returnPtsWonPct, df$l_returnPtsWonPct)




# ----------------- Create Difference Variables -----------------
# ranking difference
df$rank_diff <- df$A_rank - df$B_rank

# Ranking points difference
df$rank_points_diff <- df$A_rank_points - df$B_rank_points

# Ace differential
df$ace_diff <- df$A_ace - df$B_ace

# Double Fault differential
df$df_diff <- df$A_df - df$B_df

# Service Points differential
df$svpt_diff <- df$A_svpt - df$B_svpt

# 1st serves in differentail
df$diff_1stIn <- df$A_1stIn - df$B_1stIn

# 1st serve points won differential
df$diff_1stWon <- df$A_1stWon - df$B_1stWon

# 2nd serve points won differential
df$diff_2ndWon <- df$A_2ndWon - df$B_2ndWon

# Service Games Differential
df$SvGms_diff <- df$A_SvGms - df$B_SvGms

# Break Points Saved Differential
df$bpSaved_diff <- df$A_bpSaved - df$B_bpSaved

# Break Points faced differential
df$bpFaced_diff <- df$A_bpFaced - df$B_bpFaced

# Break points created differential
df$bpCreated_diff <- df$A_bpCreated - df$B_bpCreated

# Break points won differential
df$bpConv_diff <- df$A_bpConv - df$B_bpConv

# 1st serve percentage difference
df$diff_1stInPct <- 
  (df$A_1stInPct - df$B_1stInPct)

# 1st serve points won % difference
df$diff_1stWonPct <- 
  (df$A_1stWonPct - df$B_1stWonPct)

# 2nd serve points won % difference
df$diff_2ndWonPct <-
  (df$A_2ndWonPct - df$B_2ndWonPct)

# Break Points saved % difference
df$bpSavedPct_diff <-
  (df$A_bpSavedPct - df$B_bpSavedPct)

# Break point conversion rate difference
df$bpConvPct_diff <- 
  (df$A_bpConvPct - df$B_bpConvPct)

# return points won difference
df$returnPtsWon_diff <- 
  (df$A_returnPtsWon - df$B_returnPtsWon)

# Return points won % difference 
df$returnPtsWonPct_diff <-
  (df$A_returnPtsWonPct - df$B_returnPtsWonPct)



# ------------ Standardizing the Diffrence Variables ---------------
# Creating vector that contains all of the difference variable names

diff_vars <- grep("^diff|_diff$", names(df), value = TRUE)

# Standardizing the diffrence variables
df[diff_vars] <- scale(df[diff_vars])


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

# ---------------------------------------------------
# global.R MUST contain:
#   df, train_clay, train_grass, train_hard
#   + all preprocessing steps you already wrote
# ---------------------------------------------------



# All difference variables (created in global.R)
diff_vars <- grep("^diff|_diff$", names(df), value = TRUE)

# Allowed surfaces
surface_choices <- c("Clay", "Grass", "Hard")

# -------------------------------------------------------
# Descriptive Labels for Independent Variables (diff vars)
# -------------------------------------------------------

var_labels <- c(
  "ace_diff"             = "Ace Differential",
  "df_diff"              = "Double Fault Differential",
  "svpt_diff"            = "Service Points Differential",
  "diff_1stIn"           = "1st Serves In Differential",
  "diff_1stWon"          = "1st Serve Points Won Differential",
  "diff_2ndWon"          = "2nd Serve Points Won Differential",
  "SvGms_diff"           = "Service Games Differential",
  "bpSaved_diff"         = "Break Points Saved Differential",
  "bpFaced_diff"         = "Break Points Faced Differential",
  "bpCreated_diff"       = "Break Points Created Differential",
  "bpConv_diff"          = "Break Points Converted Differential",
  "diff_1stInPct"        = "1st Serve % Differential",
  "diff_1stWonPct"       = "1st Serve Points Won % Diff",
  "diff_2ndWonPct"       = "2nd Serve Points Won % Diff",
  "bpSavedPct_diff"      = "Break Points Saved % Differential",
  "bpConvPct_diff"       = "Break Point Conversion % Differential",
  "returnPtsWon_diff"    = "Return Points Won Differential",
  "returnPtsWonPct_diff" = "Return Points Won % Differential",
  "rank_diff"            = "Ranking Differential",
  "rank_points_diff"     = "Ranking Points Differential"
)

# Automatically label any missing diff variables:
missing <- setdiff(diff_vars, names(var_labels))
var_labels[missing] <- missing  # fallback: use variable name itself




ui <- fluidPage(
  
  titlePanel("Logistic Regression Explorer by Surface"),
  
  sidebarLayout(
    sidebarPanel(
      
      # MULTIPLE SURFACES ALLOWED
      selectInput("surface", "Select Surface:",
                  choices = surface_choices,
                  multiple = TRUE,
                  selected = c("Clay", "Grass", "Hard")),
      
      # DEPENDENT VARIABLE IS FIXED
      selectInput("dep_var", "Dependent Variable:",
                  choices = "match_result",
                  selected = "match_result"),
      
      # MULTIPLE INDEPENDENT VARIABLES FROM diff VARS
      selectInput("indep_vars", "Independent Variables:",
                  choices = diff_vars,
                  multiple = TRUE,
                  selected = c("ace_diff", "df_diff", "diff_1stInPct",
                               "diff_2ndWonPct", "bpCreated_diff",
                               "bpConvPct_diff")),
      
      actionButton("run", "Run Logistic Regression", class = "btn-primary")
      
    ),
    
    mainPanel(
      h3("Logistic Regression Results"),
      verbatimTextOutput("model_summary")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$run, {
    
    # Validate surface selection
    if (length(input$surface) == 0) {
      output$model_summary <- renderPrint("Please select at least one surface.")
      return(NULL)
    }
    
    # Validate predictors
    if (length(input$indep_vars) == 0) {
      output$model_summary <- renderPrint("Please select at least one independent variable.")
      return(NULL)
    }
    
    # Build dynamic formula
    formula <- as.formula(
      paste0(input$dep_var, " ~ ", paste(input$indep_vars, collapse = " + "))
    )
    
    # Fit logistic models for EACH selected surface
    model_list <- lapply(input$surface, function(surf_name) {
      
      df_surf <- switch(
        surf_name,
        "Clay"  = train_clay,
        "Grass" = train_grass,
        "Hard"  = train_hard
      )
      
      glm(formula, data = df_surf, family = binomial)
    })
    
    # Output side-by-side table using stargazer
    output$model_summary <- renderPrint({
      stargazer(
        model_list,
        type = "text",
        column.labels = input$surface,
        title = "Logistic Regression Output for Specified Surfaces and Variables",
        dep.var.labels = input$dep_var,
        covariate.labels = var_labels[input$indep_vars],
        digits = 2
      )
    })
    
  })
}

shinyApp(ui = ui, server = server)
