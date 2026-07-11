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

url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=2026&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=13&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2026&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


as_tibble(json_resp$resultSets$rowSet[[1]],
          .name_repair = ~json_resp$resultSets$headers[[1]]) %>%
  retype() %>%
  clean_names() %>%
  write.csv(paste0("data/summer_", gsub("-", "_", today), ".csv"), row.names = F)


url_salt <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=2026&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=16&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2026&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=&VsConference=&VsDivision=&Weight="


res <- GET(url = url_salt, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


as_tibble(json_resp$resultSets$rowSet[[1]],
          .name_repair = ~json_resp$resultSets$headers[[1]]) %>%
  retype() %>%
  clean_names() %>%
  write.csv(paste0("data/summer_saltlake_", gsub("-", "_", today), ".csv"), row.names = F)
