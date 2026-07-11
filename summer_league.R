library(tidyverse)
library(hoopR)
library(janitor)
library(hablar)
library(gt)
library(gtExtras)
library(gtUtils)
library(httr)
library(jsonlite)


twitter <- "<span style='color:#b14f04'>&#x1D54F;</span>"
tweetelcheff <- "<span style='font-weight:bold;color: grey;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;color: grey;'>*@sport_iv0*</span>"
github <- "<span style='color:#c8102e;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;color: grey;'>*IvoVillanueva*</span>"
caption <- glue::glue("**Datos**: *@NBA* | **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • The Clean Shot")


# calendario --------------------------------------------------------------

headers <- c(
  "ocp-apim-subscription-key" = "747fa6900c6c4e89a58b81b72f36eb96",
  "origin" = "https://www.nba.com",
  "referer" = "https://www.nba.com/",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"
)

fecha <- format(Sys.Date() - 1, "%m/%d/%Y")
score_url <- paste0("https://core-api.nba.com/cp/api/v1.9/feeds/gamecardfeed?gamedate=", fecha, "&platform=web")

res_score <- GET(url = score_url, add_headers(.headers=headers))
raw <- fromJSON(content(res_score, "text"), simplifyVector = FALSE)

games <- map_dfr(raw$modules[[1]]$cards, function(c) {
  g <- c$cardData
  tibble(
    gameId = g$gameId,
    status = g$gameStatusText,
    home   = g$homeTeam$teamTricode,
    homePts = g$homeTeam$score,
    away   = g$awayTeam$teamTricode,
    awayPts = g$awayTeam$score
  )
})

calendario <- games %>%
  rename(team_abbreviation = home,
         versus = away,
         puntos = homePts,
         puntos_versus = awayPts) %>%
  rbind(games %>%
          rename(team_abbreviation = away,
                 versus = home,
                 puntos = awayPts,
                 puntos_versus = homePts)) %>%
  mutate(dif = puntos - puntos_versus,
         fancy_scor = paste0(
           "vs. ", versus, " ",
           ifelse(dif > 0,
                  paste0("<span style='color:forestgreen'>(W +", dif, ")</span>"),
                  paste0("<span style='color:firebrick'>(L ", dif, ")</span>"))
         )
  )



read_csv("data/summer_vegas_2026_07_11.csv") %>%
  #rbind(read_csv("data/summer_saltlake_2026_07_08.csv")) %>%
  retype() %>%
  clean_names() %>%
  left_join(calendario, by = join_by(team_abbreviation)) %>%
  filter(!is.na(fancy_scor)) %>%
  transmute(
    logo = case_when(team_abbreviation == "GWG" ~ "GSW",
      team_abbreviation == "GWB" ~ "GSW",
      .default = team_abbreviation
    ),
    logo = paste0("https://raw.githubusercontent.com/IvoVillanueva/NBA/refs/heads/main/logos_cuadrados/", logo, ".png"),
    player_name,
    fancy_scor,
    gp,
    min,
    pts,
    reb,
    ast,
    tov,
    stl,
    blk,
    pf,
    fgm,
    fga,
    fg3m,
    fg3a,
    ftm,
    fta,
    plus_minus,
    #dre = (.79 * pts - .72 * (fga - fg3a) - .55 * fg3a - .16 * fta + .13 * oreb + .40 * dreb + .54 * ast + 1.68 * stl + .76 * blk - 1.36 * tov - .11 * pf),
    val = (pts + reb + ast + stl + blk + pfd) -
      (fga - fgm) - (fta - ftm) - tov - blka - pf
  ) %>%
  mutate(
    fgm = paste0(fgm, "/", fga),
    fg3m = paste0(fg3m, "/", fg3a),
    ftm = paste0(ftm, "/", fta)
  ) %>%
  arrange(desc(val)) %>%
  # take top 20
  filter(row_number() <= 20) %>%
  gt() %>%
  tab_header(
    title = md("**Best Rookies en Las Vegas Segundo Día**"),
    subtitle = "Los 20 mejores en Valoración FIBA de Las Vegas Summer League"
  ) %>%
 gt_merge_stack(player_name, fancy_scor) %>%
  tab_source_note(
    source_note = md(caption)
  ) %>%
  cols_label(
    logo = "",
    player_name = "Player",
    gp = "GP",
    min = "MIN",
    pts = "PTS",
    reb = "REB",
    ast = "AST",
    tov = "TOV",
    stl = "STL",
    blk = "BLK",
    fgm = "FGM/A",
    fg3m = "3PM/A",
    ftm = "FTM/A",
    plus_minus = "+/-",
    val = "VAL"
  ) %>%
  fmt_number(plus_minus, force_sign = T, decimals = 0) %>%
  fmt_number(val, decimals = 1) %>%
  gt_img_rows(columns = logo, height = 30) %>%
  data_color(
    columns = "val",
    #alpha = .75,
    reverse = F,
    palette = c("white", "#1D428A")
  ) %>%
  tab_options(
    data_row.padding = "0px",
    table.font.names = "Bebas Neue",
    table_body.hlines.color = "transparent",
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.style = "none",
    column_labels.font.weight = "strong",
    row_group.border.top.style = "none",
    row_group.border.top.color = "black",
    row_group.border.bottom.width = px(1),
    row_group.border.bottom.color = "black",
    row_group.border.bottom.style = "solid",
    row_group.padding = px(1.5),
    heading.align = "left",
    heading.border.bottom.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "white",
    table.border.bottom.style = "none",
    table.border.top.style = "none",
    source_notes.border.lr.style = "none"
  ) %>%
  # save table
  gt_save_crop("png/summer_league_las_vegas_rookies_day2.png", whitespace = 100, zoom = 2)
