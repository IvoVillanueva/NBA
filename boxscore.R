
library(rvest)


headers <- c(
  "ocp-apim-subscription-key" = "747fa6900c6c4e89a58b81b72f36eb96",
  "origin" = "https://www.nba.com",
  "referer" = "https://www.nba.com/",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"
)

get_gamecards <- function(fecha) {
  fecha <- format(fecha, "%m/%d/%Y")
  score_url <- paste0("https://core-api.nba.com/cp/api/v1.9/feeds/gamecardfeed?gamedate=", fecha, "&platform=web")
  res_score <- GET(url = score_url, add_headers(.headers = headers))
  raw <- fromJSON(content(res_score, "text"), simplifyVector = FALSE)

  raw$modules[[1]]$cards %>%
    map_dfr(function(c) {
      g <- c$cardData
      tibble(
        gameId = g$gameId,
        fecha  = g$gameTimeEastern,
        status = g$gameStatusText,
        home   = g$homeTeam$teamTricode,
        homePts = g$homeTeam$score,
        away   = g$awayTeam$teamTricode,
        awayPts = g$awayTeam$score
      )
    })
}


fechas <- c(Sys.Date() - 2, Sys.Date() - 1)
games <- map_dfr(fechas, get_gamecards)


historico <- read.csv("data/summer_games.csv")
nuevo <- get_gamecards(Sys.Date() - 1)
bind_rows(historico, nuevo) %>%
  distinct(gameId, .keep_all = TRUE) %>%
  write.csv("data/summer_games.csv", row.names = FALSE)

GET("https://cdn.nba.com/static/json/liveData/boxscore/boxscore_1522600001.json") %>% content("text") %>% substr(1, 200)


"https://www.nba.com/game/min-vs-nop-1522600001/box-score" %>%
  read_html() %>%
  html_table()
