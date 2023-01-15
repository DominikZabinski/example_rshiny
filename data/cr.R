# creating database (run from sample.Rproj)
# libraries ----
library(DBI)
library(data.table)

# reading data ----
musicPeople <- data.table(read.csv2(file = "../data/data.csv", stringsAsFactors = F))

# creating dictionairies ----
genreDict <- data.table(genre = unique(musicPeople$genre))
genreDict$genre_id <- 1:nrow(genreDict)

typeDict <- data.table(type = unique(musicPeople$type))
typeDict$type_id <- 1:nrow(typeDict)

# leaving only necessary columns in the main table ----
musicPeople$id <- 1:nrow(musicPeople)
musicPeople <- musicPeople[typeDict, on = .(type)][genreDict, on = .(genre)][,.(id, name, genre_id, type_id)]

# creating random relationships ----
rels <- data.table(expand.grid(idA = musicPeople$id, idB = musicPeople$id))
rels[, id_A := pmin(idA, idB)]
rels[, id_B := pmax(idA, idB)]
rels <- unique(rels[id_A != id_B][,.(id_A, id_B)])
rels <- rels[sample(1:nrow(rels), size = ceiling(.15 * nrow(rels)))]
rels$id <- 1:nrow(rels)

# saving data in .sqlite database ----
con <- dbConnect(drv = RSQLite::SQLite(), "../data/mydatabase.sqlite")
dbWriteTable(conn = con, name = "music_people", value = musicPeople)
dbWriteTable(conn = con, name = "type_dict", value = typeDict)
dbWriteTable(conn = con, name = "genre_dict", value = genreDict)
dbWriteTable(conn = con, name = "rels", value = rels)

dbDisconnect(conn = con)

file.copy(from = "../data/mydatabase.sqlite", to = "mydatabase.sqlite", overwrite = T)
