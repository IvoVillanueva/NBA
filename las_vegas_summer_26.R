library(dplyr)
library(janitor)
library(hablar)
library(httr)
library(jsonlite)

# get today's date
today <- Sys.Date()

# ensure data output folder exists
if (!dir.exists("data")) dir.create("data")

headers <- c(
  "accept" = "*/*",
  "accept-encoding" = "gzip, deflate, br, zstd",
  "accept-language" = "es-ES,es;q=0.9,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "origin" = "https://www.nba.com",
  "pragma" = "no-cache",
  "referer" = "https://www.nba.com/",
  "sec-ch-ua" = "\"Google Chrome\";v=\"149\", \"Chromium\";v=\"149\", \"Not)A;Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"macOS\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-site",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"
)

url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=2026&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=15&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2026&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


as_tibble(json_resp$resultSets$rowSet[[1]],
          .name_repair = ~json_resp$resultSets$headers[[1]]) %>%
  retype() %>%
  clean_names() %>%
  write.csv(paste0("data/summer_vegas_", gsub("-", "_", today), ".csv"), row.names = F)


gamelogs <- nba_leaguegamelog(season = "2026-27", league_id = 15, player_or_team = 'P')



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

games %>%
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
                  paste0("<span style='color:forestgreen'>(W+", dif, ")</span>"),
                  paste0("<span style='color:firebrick'>(L", dif, ")</span>"))
         )
  )
