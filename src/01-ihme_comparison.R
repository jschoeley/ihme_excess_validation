# Compare implicit IHME expected deaths with past trends and other estimates
#
# Jonas Schöley

# Init ------------------------------------------------------------

library(yaml); library(tidyverse); library(readODS)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  ihme_sources = './dat/estimate_comparison.ods',
  global_objects = './src/00-global_objects.R'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  data = './out/output_data.rds',
  ihme_expected = './out/'
)


# global objects
source(paths$input$global_objects)

# Input -----------------------------------------------------------

comparison <-
  read_ods(
    paths$input$ihme_sources, sheet = 'comparison_biannual', na = '.'
  ) %>%
  mutate(years_integer = as.integer(as.factor(years))) %>%
  mutate(across(
    c(observed_deaths, observed_deaths_leap_year_adjusted, ihme_excess,
      ihme_expected, economist_expected, wmd_expected),
    ~./1e3
  ))

# Plot ------------------------------------------------------------

fig <- list()

fig$comparison <- 
  comparison %>%
  filter(
    country %in% c('Belgium', 'Denmark', 'Germany', 'Japan', 'Portugal',
                   'Spain', 'Kazakhstan')
  ) %>%
  ggplot() +
  aes(x = years_integer, y = observed_deaths) +
  geom_line(data = . %>% filter(years != '2020/21')) +
  geom_line(
    data =
      . %>% filter(years %in% c('2018/19', '2020/21')) %>%
      mutate(observed_deaths =
               ifelse(years == '2020/21', ihme_expected, observed_deaths)),
    color = 'red', lty = 3
  ) +
  # ihme excess
  geom_linerange(
    aes(x = years_integer, ymin = ihme_expected, ymax = observed_deaths),
    data = . %>% filter(years == '2020/21'), color = 'red'
  ) +
  # observed
  geom_point(size = 5, color = 'grey95') +
  geom_point(
    size = 5, color = 'grey95', aes(y = ihme_expected),
    data = . %>% filter(years == '2020/21')
  ) +
  geom_point(
    aes(x = years_integer, y = ihme_expected), color = 'red',
    data = . %>% filter(years == '2020/21')
  ) +
  # geom_point(
  #   size = 5, color = 'grey95', aes(y = economist_expected),
  #   data = . %>% filter(years == '2020/21')
  # ) +
  geom_point(
    aes(x = years_integer, y = economist_expected), color = 'blue',
    shape = 21,
    data = . %>% filter(years == '2020/21')
  ) +
  # geom_point(
  #   shape = 4, size = 5, color = 'grey95', aes(y = wmd_expected),
  #   data = . %>% filter(years == '2020/21')
  # ) +
  geom_point(
    aes(x = years_integer, y = wmd_expected), color = 'blue',
    shape = 4,
    data = . %>% filter(years == '2020/21')
  ) +
  # % diff label
  geom_label(
    aes(
      x = x,
      y = y,
      label = lab
    ),
    label.padding = unit(1, 'pt'), label.size = 0, fill = 'grey95',
    data = . %>%
      mutate(
        x = years_integer+0.5,
        y = (observed_deaths+lead(observed_deaths))/2,
        lab = paste0(formatC(lead(observed_deaths_biannual_pct_change),
                             digits = 2, flag = '+'), '%')
      ) %>%
      filter(!years %in% c('2018/19', '2020/21')),
    family = 'Roboto Condensed',
    size = 2.8, lineheight = 0.8
  ) +
  geom_label(
    aes(
      x = x,
      y = y,
      label = lab
    ),
    label.padding = unit(1, 'pt'), label.size = 0, fill = 'grey95',
    color = 'red',
    data = . %>%
      mutate(
        x = years_integer+0.5,
        y = (observed_deaths+lead(ihme_expected))/2,
        lab = paste0(formatC(lead(ihme_expected_biannual_pct_change),
                             digits = 2, flag = '+'), '%')
      ) %>%
      filter(years %in% c('2018/19')),
    family = 'Roboto Condensed',
    size = 2.8, lineheight = 0.8
  ) +
  geom_text(
    aes(
      x = years_integer+0.1,
      y = observed_deaths,
      label = paste0(observed_deaths, 'k', '\nobserved deaths')
    ),
    hjust = 0, vjust = 0, color = 'black',
    data = . %>% filter(years == '2020/21'),
    family = 'Roboto Condensed',
    size = 2.8, lineheight = 0.8
  ) +
  geom_text(
    aes(
      x = years_integer + 0.1,
      y = (observed_deaths+ihme_expected)/2,
      label = paste0(ihme_excess, 'k', '\nIHME excess')
    ),
    hjust = 0, vjust = 0.5,
    color = 'red',
    data = . %>% filter(years == '2020/21'),
    family = 'Roboto Condensed',
    size = 2.8, lineheight = 0.8
  ) +
  geom_text(
    aes(
      x = years_integer+0.1,
      y = ihme_expected,
      label = paste0('implies ', ihme_expected, 'k', '\nexpected deaths')
    ),
    hjust = 0, vjust = 1,
    data = . %>% filter(years == '2020/21'),
    family = 'Roboto Condensed',
    size = 2.8, lineheight = 0.8, color = 'red'
  ) +
  geom_point() +
  facet_wrap(~country, scales = 'free_y', ncol = 2) +
  scale_x_continuous(
    breaks = 1:11,
    labels = c('00/01', '02/03', '04/05', '06/07', '08/09',
               '10/11', '12/13', '14/15', '16/17', '18/19',
               '20/21'),
    limits = c(1, 13)
  ) +
  scale_y_continuous(expand = c(0.17, 0)) +
  labs(
    y = 'Deaths (thousands)',
    x = 'Period',
    #title = 'IHME excess death estimates imply implausible numbers of expected deaths',
    #subtitle = 'In some countries, for the IHME excess deaths estimate to be true, expected deaths must break with past trends',
    #caption = 'Observed deaths by STMF (https://www.mortality.org/Public/STMF/Outputs/stmf.xlsx) accessed 2022-04-07\nObserved deaths Japan pre 2015 by https://www.stat.go.jp/data/nenkan/71nenkan/zuhyou/y710216000.xlsx acessed 2022-04-25 and since 2015 by WMD\nEconomist expected by https://github.com/TheEconomist/covid-19-excess-deaths-tracker/raw/master/output-data/alternative-exports-by-non-iso-week/excess-deaths acessed 2022-04-06\nIHME excess by https://www.thelancet.com/action/showFullTableHTML?isHtml=true&tableId=tbl1&pii=S0140-6736%2821%2902796-3 acessed 2022-04-06\nJonas Schöley @jschoeley'
  ) +
  figspec$MyGGplotTheme(size = 10) +
  theme(
    panel.background = element_rect(fill = 'grey95', color = NA),
    # plot
    title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "black", size = 11, face = "plain")
  )
fig$comparison

# Export ----------------------------------------------------------

# export results of analysis
ExportFigure(
  fig$comparison,
  path = paths$output$ihme_expected,
  device = 'svg',
  filename = 'ihme_expected', width = 160, height = 130, scale = 1.7
)
