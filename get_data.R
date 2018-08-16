# getting football data from different divisions
# reading in data
# data comes from: football-data.co.uk
# links build up as: http://www.football-data.co.uk/mmz4281/SEASON/..csv
# notes on data: http://www.football-data.co.uk/notes.txt
# England premier league: E0; championship E1; ...
# Netherlands N1;

# Get data for seasons 2015/2016, 2016/2017, 2017/2018 and 2018/2019.
england <- list(s1516 = 
                  list(E0 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1516/E0.csv")),
                    E1 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1516/E1.csv")),
                    E2 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1516/E2.csv"))
                  ),
                s1617 = 
                  list(E0 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1617/E0.csv")),
                    E1 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1617/E1.csv")),
                    E2 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1617/E2.csv"))
                  ),
                s1718 = list(E0 = read.csv(url(
                  "http://www.football-data.co.uk/mmz4281/1718/E0.csv")),
                  E1 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1718/E1.csv")),
                  E2 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1718/E2.csv"))
                ),
                s1819 = list(E0 = read.csv(url(
                  "http://www.football-data.co.uk/mmz4281/1819/E0.csv")),
                  E1 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1819/E1.csv")),
                  E2 = read.csv(url(
                    "http://www.football-data.co.uk/mmz4281/1819/E2.csv"))
                ))

netherlands <- list(s1516 = 
                      list(E0 = read.csv(url(
                        "http://www.football-data.co.uk/mmz4281/1516/N1.csv"))
                      ),
                    s1617 = 
                      list(E0 = read.csv(url(
                        "http://www.football-data.co.uk/mmz4281/1617/N1.csv"))
                      ),
                    s1718 = list(E0 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1718/N1.csv"))
                    ),
                    s1819 = list(E0 = read.csv(url(
                      "http://www.football-data.co.uk/mmz4281/1819/N1.csv"))
                    ))

