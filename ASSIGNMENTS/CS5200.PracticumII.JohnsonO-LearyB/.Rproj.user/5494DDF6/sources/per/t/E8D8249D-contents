
#' Bridget Leary and Omi Johnson, Practicum 2
#' CS 5200 with Martin Schedlbauer, Fall 2022
#' 12/7/2022

library(RMySQL)
library(RSQLite)

# QUESTION 2.2 - CONNECTING TO MySQL AND SQLite
# ---------------------------------------------
dw <-  dbConnect(MySQL(), 
                 user = 'root', 
                 password = '587DJLOTL4mys',
                 dbname = 'starschema', 
                 host = 'localhost', 
                 port = 3306)

db <- dbConnect(SQLite(), "pubmedArticleSet.db")

# helper functions to simplify executing and retrieving
doSQL <- function(...) {
  dbExecute(dw, paste(...))
}
getQuery <- function(...) {
  dbGetQuery(db, paste(...))
}

doSQL("SET GLOBAL local_infile = 1;")

# QUESTION 2.3 - CREATE STAR SCHEMA FOR AUTHOR FACTS
# ----------------------------------------------------

doSQL("DROP TABLE IF EXISTS starschema.AuthorshipDimension;")
doSQL("DROP TABLE IF EXISTS starschema.AuthorFact;")

# CREATING SQL TABLES

doSQL(
  "CREATE TABLE starschema.AuthorFact (",
  "aid INT NOT NULL PRIMARY KEY,",
  "ValidYN BOOLEAN NOT NULL,",
  "CollectiveName TEXT,",
  "LastName TEXT,",
  "ForeName TEXT,",
  "Initials TEXT,",
  "Suffix TEXT,",
  "Affiliation TEXT,",
  "numArticles INT,",
  "numCoAuthors INT,",
  "numJournals INT",
  ");"
)

doSQL(
  "CREATE TABLE starschema.AuthorshipDimension (",
  "aid INT NOT NULL PRIMARY KEY,",
  "ArticleTitle TEXT NOT NULL,",
  "ArticleLanguage TEXT,",
  "PubYear INT,",
  "PubMonth INT,",
  "PubDay INT,",
  "JournalISSN TEXT,",
  "JournalISSNType TEXT,",
  "JournalISO TEXT,",
  "Volume INT,",
  "Issue INT",
  ")"
)

# CREATING authorDF

authorDF <- 
  getQuery("WITH coAuthors AS (SELECT a1.aid,",
                                     "COUNT(DISTINCT a2.aid) - 1 AS numCoAuthors",
                                "FROM Authorship a1",
                               "INNER JOIN Authorship a2 ON (a1.PMID = a2.PMID)",
                               "GROUP BY a1.aid)",
           "SELECT au.*, COUNT(DISTINCT r.PMID) AS numArticles, ca.numCoAuthors,",
                  "COUNT(DISTINCT ji.JournalID) AS numJournals",
             "FROM Authorship aus",
            "INNER JOIN Author au ON (au.aid = aus.aid)",
            "INNER JOIN coAuthors ca ON (ca.aid = aus.aid)",
            "INNER JOIN Article r ON (aus.PMID = r.PMID)",
            "INNER JOIN JournalIssue ji ON (ji.jiid = r.JournalIssueID)",
            "GROUP BY au.aid")

dbWriteTable(dw, "AuthorFact", authorDF, 
             append = TRUE, row.names = FALSE)

# CREATING authorshipDimensionDF

authorshipDimensionDF <- 
  getQuery("SELECT a.aid, r.ArticleTitle, r.Language AS ArticleLanguage,",
           "i.PubYear, i.PubMonth, i.PubDay, j.ISSN AS JournalISSN,",
           "j.ISSNType AS JournalISSNType, j.ISOAbbreviation AS JournalISO,",
           "i.Volume, i.Issue",
           "FROM Authorship a",
           "JOIN Article r ON a.PMID=r.PMID",
           "JOIN JournalIssue i ON i.jiid = r.JournalIssueID",
           "JOIN Journal j ON j.jid=i.JournalId;")

dbWriteTable(dw, "AuthorshipDimension", authorshipDimensionDF, 
             append = TRUE, row.names = FALSE)

# QUESTION 2.4 - CREATING JOURNAL STAR SCHEMA
# ------------------------------------------------------

doSQL("DROP TABLE IF EXISTS starschema.PublicationDimension;")
doSQL("DROP TABLE IF EXISTS starschema.JournalFact;")

# INTIIALIZING SQL TABLES

doSQL("CREATE TABLE starschema.JournalFact (",
      "jid INTEGER NOT NULL PRIMARY KEY,",
      "ISSN TEXT,",
      "ISSNType TEXT,",
      "Title TEXT,",
      "ISOAbbreviation TEXT,",
      "numIssues INTEGER,",
      "numVolumes INTEGER, ",
      "numArticles INTEGER,",
      "numAuthors INTEGER",
      ");")

doSQL("CREATE TABLE starschema.PublicationDimension (",
      "jid INTEGER NOT NULL,",
      "PMID INTEGER NOT NULL,",
      "Language TEXT NOT NULL,", 
      "ArticleTitle TEXT NOT NULL,",
      "aid INTEGER NOT NULL,",
      "aLastName TEXT,",
      "aForeName TEXT,",
      "aaffiliation TEXT,",
      "Volume INTEGER,",
      "Issue INTEGER,",
      "PubYear INTEGER NOT NULL,",
      "PubMonth INTEGER NOT NULL,",
      "PubDay INTEGER,",
      "PubQuarter INTEGER,",
      "FOREIGN KEY (jid) REFERENCES JournalFact(jid)",
      ");")

# CONSTRUCTING JournalFactDF

journalFactDF <- 
  getQuery("SELECT j.*, d.numIssues, d.numVolumes, d.numArticles, d.numAuthors",
             "FROM Journal j",
            "INNER JOIN (SELECT ji.JournalID AS jid,",
                               "COUNT(DISTINCT ji.Issue) AS numIssues,",
                               "COUNT(DISTINCT ji.Volume) AS numVolumes,",
                               "COUNT(DISTINCT ar.PMID) AS numArticles,",
                               "COUNT(DISTINCT au.aid) AS numAuthors", 
                          "FROM Article ar",
                          "LEFT JOIN Authorship au ON (au.PMID = ar.PMID)",
                        "INNER JOIN JournalIssue ji ON (ji.jiid = ar.JournalIssueID)",
                        "GROUP BY ji.JournalID) AS d",
               "ON (j.jid = d.jid)")

dbWriteTable(dw, "JournalFact", journalFactDF, append = TRUE,
             row.names = FALSE)

doSQL("UPDATE JournalFact SET numAuthors = 0 WHERE numAuthors IS NULL")

# CONSTRUCTING PublicationDimensionDF

publicationDimensionDF <- 
  getQuery("SELECT ji.JournalID AS jid, ar.PMID, ar.Language, ar.ArticleTitle,",
                  "aus.aid, au.ForeName AS aForeName, au.LastName AS aLastName,",
                  "au.affiliation AS aaffiliation, ji.Volume, ji.Issue,",
                  "ji.PubYear, ji.PubMonth, ji.PubDay, CEIL(PubMonth/3.0) AS PubQuarter",
             "FROM Article ar",
             "LEFT JOIN Authorship aus ON (aus.PMID = ar.PMID)",
             "LEFT JOIN Author au ON (aus.aid = au.aid)",
            "INNER JOIN JournalIssue ji ON (ji.jiid = ar.JournalIssueID)")

dbWriteTable(dw, "PublicationDimension", publicationDimensionDF, 
             append = TRUE, row.names = FALSE)

dbDisconnect(dw)
dbDisconnect(db)
