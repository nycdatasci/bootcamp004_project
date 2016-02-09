source('helpers.R')

library(shinyBS)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = 'NBA Lineup Data'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lineups", tabName = "lineups", icon=icon('line-chart')),
      menuItem("Diminising Returns", tabName = 'diminishing', icon=icon('sort-amount-desc')),
      menuItem("Distributions", tabName = 'density', icon=icon('area-chart'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "lineups",
        fluidRow(
          column(
            width=12,
            box(
              width=12,
              plotOutput("lineup_plot")
            ),
            box(
              title="Lineups",
              width=12,
              collapsible=TRUE,
              tags$div(
                class='header',
                tags$p(
                  'This app displays the performance of NBA Lineups (5 man combinations) relative to',
                  'the full-season performance of the players who made up that lineup.',
                  'The goal is to understand the underlying relationships between individual player',
                  'performance and the performance of a given 5 man unit.'
                ),
                tags$p(
                  'Please select a', tags$em('Lineup Stat'), 'to display on the Y axis.',
                  'A', tags$em('Lineup Stat'), 'tells us the performance on a given metric of the 5 players in a',
                  'given lineup while they shared the floor.'
                ),
                tags$p(
                  'Please select a', tags$em('Player Stat'), 'to display on the X axis.',
                  'A', tags$em('Player Stat'), 'tells us the average performance on a given metric of the 5 players',
                  'in a given lineup, regardless of whether they were playing together or not.'
                ),
                tags$p(
                  'If you elect to use Usage-Weighted Stats, the player averages will be calculated using the ',
                  'players\'', tags$em('Usage Percentages'), 'as weights.'
                ),
                tags$p(
                  'All data from', tags$a(href='http://stats.nba.com', 'the NBA.'), 'Stat definitions can be found',
                  'on', tags$a(href='http://stats.nba.com', 'their site.')
                )
              )
            ),
            box(
              width=4,
              selectInput("lineup_stat",
                          "Lineup Stat:",
                          lineup_stats,
                          selected="NET_RATING"),
              bsTooltip('lineup_stat', lineup_stat_tip),
              selectInput("player_stat",
                          "Player Stat:",
                          player_stats,
                          selected="NET_RATING"),
              bsTooltip('player_stat', player_stat_tip), 
              checkboxInput('usage', "Use Usage-Weighted Stats?"),
              bsTooltip('usage', usage_tip)
            ),
            box(
              width=8,
              verbatimTextOutput('lineup_text')
            ),
            box(
              width=12,
              sliderInput("minute_range", "Lineup Minute Range:",
                          step=5, min = 0, max = max_minutes, value = c(10,1000)),
              bsTooltip('minute_range', placement='top', minute_filter_tip)
            )
          )
        )
      ),
      tabItem(
        tabName = 'diminishing',
        fluidRow(
          column(
            width=12,
            box(
              width=12,
              plotOutput("diminishing_plot")
            ),
            box(
              width=12,
              title='Diminishing Returns',
              collapsible=TRUE,
              tags$div(class='header',
                tags$p(
                  'These diminishing returns plots are inspired by',
                  tags$a(href='http://www.countthebasket.com/blog/2008/03/06/diminishing-returns-for-scoring-usage-vs-efficiency/', 'Eli Witus.')
                ),
                tags$p(
                  'For most offensive stats, I suggest selecting Usage-Weighted Stats.',
                  'The Y axis will display the difference between the performance we might expect if there were',
                  'no dimishing returns and the actual performance.',
                  'The X axis displays how much the players in a lineup were forced to decrease or increase',
                  'their usage beyond their norms.',
                  'An increasing slope (for desireable stats) suggests diminishing returns.'
                ),
                tags$p(
                  'For other stats, such as rebounding, Usage-Weighting will not make much sense.',
                  'If you do not select to use Usage-Weighted stats, we still plot the deviation from what we might',
                  'expect in the absense of diminishing returns on the Y axis, but the X axis displays our expectation.'
                ),
                tags$p(
                  'For \'counting stats\', please select to Sum Player Stats.',
                  'This will multiply the averages by 5, reflecting the expectation that these would be additive',
                  'for a 5 man lineup.'
                )
              )
            ),
            box(
              width=4,
              selectInput("diminishing_stat",
                          "Stat:",
                          both_stats,
                          selected="TS_PCT"),
              bsTooltip('diminishing_stat', dim_stat_tip),
              checkboxInput('dim_usage', "Use Usage-Weighted Stats?", value=TRUE),
              bsTooltip('dim_usage', usage_tip),
              checkboxInput('dim_sum', 'Sum Player Stats?'),
              bsTooltip('dim_sum', sum_tip)
            ),
            box(
              width=8,
              verbatimTextOutput('diminishing_text')
            ),
            box(
              width=12,
              sliderInput("dim_minute_range", "Lineup Minute Range:",
                          step=5, min = 0, max = max_minutes, value = c(10,1000)),
              bsTooltip('dim_minute_range', placement='top', minute_filter_tip)
            )
          )
        )
      ),
      tabItem(
        tabName = 'density',
        fluidRow(
          column(
            width=12,
            box(
              width=6,
              plotOutput("density_lineup_plot")
            ),
            box(
              width=6,
              plotOutput("density_player_plot")
            ),
            box(
              width=12,
              title='Density Plots',
              collapsible=TRUE,
              tags$div(
                tags$p(
                  'Here we can inspect the underlying distribution of our data.'
                )
              )
            ),
            box(
              width=4,
              selectInput("density_stat",
                          "Stat:",
                          both_stats,
                          selected="NET_RATING"),
              bsTooltip('density_stat', density_stat_tip),
              checkboxInput('den_usage', "Use Usage-Weighted Stats?"),
              bsTooltip('den_usage', usage_tip)
            ),
            box(
              width=8,
              verbatimTextOutput('density_text')
            ),
            box(
              width=12,
              sliderInput("density_minute_range", "Lineup Minute Range:",
                          step=5, min = 0, max = max_minutes, value = c(10,1000)),
              bsTooltip('dim_minute_range', placement='top', minute_filter_tip)
            )
          )
        )
      )
    )
  )
)