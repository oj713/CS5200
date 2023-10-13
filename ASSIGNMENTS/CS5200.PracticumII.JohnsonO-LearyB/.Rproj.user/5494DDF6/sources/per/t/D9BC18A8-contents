
#' Bridget Leary and Omi Johnson, Practicum 2
#' CS 5200 with Martin Schedlbauer, Fall 2022
#' 12/7/2022

# QUESTION 1.5: REALIZING THE SCHEMA
# --------------------------------------------------------------

library(RSQLite)
library(XML)
library(dplyr)

db <- dbConnect(SQLite(), "pubmedArticleSet.db")

doSQL <- function(...) {
  dbExecute(db, paste(...))
}

doSQL("PRAGMA FOREIGN_KEYS = ON")

doSQL("DROP TABLE IF EXISTS Authorship")
doSQL("DROP TABLE IF EXISTS Article")
doSQL("DROP TABLE IF EXISTS JournalIssue")
doSQL("DROP TABLE IF EXISTS Journal")
doSQL("DROP TABLE IF EXISTS Author")

doSQL("CREATE TABLE IF NOT EXISTS Journal (",
      "jid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,", 
      "ISSN TEXT,",
      "ISSNType TEXT,", 
      "Title TEXT NOT NULL,",
      "ISOAbbreviation TEXT NOT NULL,",
      "CHECK (ISSNType IN ('Print', 'Electronic'))",
      ")")

doSQL("CREATE TABLE IF NOT EXISTS JournalIssue (",
      "jiid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,", 
      "Volume INTEGER,", 
      "Issue INTEGER,",
      "PubYear INTEGER NOT NULL,",
      "PubMonth INTEGER,",
      "PubDay INTEGER,",
      "JournalID INTEGER NOT NULL,",
      "FOREIGN KEY (JournalID) REFERENCES Journal(jid)",
      ")")

doSQL("CREATE TABLE IF NOT EXISTS Article (",
      "PMID INTEGER PRIMARY KEY,",
      "Language TEXT NOT NULL,",
      "ArticleTitle TEXT NOT NULL,",
      "JournalIssueID INTEGER NOT NULL,",
      "FOREIGN KEY (JournalIssueID) REFERENCES JournalIssue(jiid)",
      ")")

doSQL("CREATE TABLE IF NOT EXISTS Author (",
      "aid INTEGER AUTO_INCREMENT PRIMARY KEY,",
      "ValidYN TINYINT(1) NOT NULL,",
      "CollectiveName TEXT,",
      "LastName TEXT,",
      "ForeName TEXT,",
      "Initials TEXT,",
      "Suffix TEXT,",
      "Affiliation TEXT)")

doSQL("CREATE TABLE IF NOT EXISTS Authorship (",
      "aid INTEGER NOT NULL,",
      "PMID INTEGER NOT NULL,",
      "FOREIGN KEY (aid) REFERENCES Author(aid),",
      "FOREIGN KEY (PMID) REFERENCES Article(PMID))")

# QUESTION 1.6: LOADING THE XML
# --------------------------------------------------------------

#xmlPath <- "pubmed-tfm-xml/pubmed-subset.xml"
xmlPath <- "pubmed-tfm-xml/pubmed22n0001-tf.xml"
xml <- xmlParse(xmlPath, validate = T)

# QUESTION 1.7: READING XML INTO DATABASES
# --------------------------------------------------------------

#' Helper that, given an xml node and an xpath command to an element, 
#' returns the value of the element as the desired type or NA if absent
getElem <- function(node, elemPath, type = "character") {
  
  elem <- xpathSApply(node, paste0(elemPath, "/text()"))
  
  if (length(elem) == 0) {
    return(NA) 
  } else {
    return(as(elem[[1]], type))
  }
}


#' JOURNAL
#' 
#' Design Decisions:
#'  - some journals use Season instead of specifying an exact
#'    month of publication. For these entries, we choose to leave Month 
#'    information blank as it cannot be determined. Season also does not map
#'    perfectly to Quarter so Quarter also cannot be determiend and the attribute
#'    is useless. 
#'  - For medline date, we assign month and year based on the 
#'    first month and year provided.
#'  - We choose to ignore the CitedMedium attribute as it is analogous to IssnType
#'  - Although we do not use ISSN as a primary key, third normal form is not 
#'    violated because some ISSNs are NULL

#' ARTICLE 
#' 
#' Design Decisions:
#'   - we transform PMID to integer rather than leaving as character
#'   - we omit the AuthorList/CompleteYN attribute

#' Helper method that takes an Article node and returns a named list of all 
#'  relevant Article, Journal, and JournalIssue attributes
processArticleJournal <- function(article) {
  articleElem <- function(elemPath, type = "character") {
    getElem(article, elemPath, type)
  }
  
  journal <- xpathSApply(article, "Journal")[[1]]
  journalElem <- function(elemPath, type = "character") {
    getElem(journal, elemPath, type)
  }
  
  # Retrieving variables requiring > 1 step
  ISSNType <- xpathSApply(journal, "ISSN/@IssnType")
  ISSNType <- if (length(ISSNType) == 0) {NA}
  else {ISSNType[[1]]}
  
  # Normalizing date to Year, Month, Day
  PubDate <- xpathSApply(journal, "JournalIssue/PubDate")[[1]]
  
  medlineDate <- getElem(PubDate, "MedlineDate")
  if (!is.na(medlineDate)) {
    Year <- substring(medlineDate, 0, 4) |> as.numeric()
    Month <- substring(medlineDate, 6, 8) |> 
      match(month.abb)
    Day <- NA
    
  } else {
    Year <- getElem(PubDate, "Year", "numeric")
    Month <- getElem(PubDate, "Month") |> 
      match(month.abb)
    Day <- getElem(PubDate, "Day", "numeric")
  }
  
  list("PMID" = xpathSApply(article, "../@PMID")[[1]] |>
         as("numeric"),
       "Language" = articleElem("Language"),
       "ArticleTitle" = articleElem("ArticleTitle"),
       "Volume" = journalElem("JournalIssue/Volume", type = "numeric"), 
       "Issue" = journalElem("JournalIssue/Issue", type = "numeric"), 
       "PubYear" = Year, 
       "PubMonth" = Month,
       "PubDay" = Day, 
       "ISSN" = journalElem("ISSN"), 
       "ISSNType" = ISSNType, 
       "Title" = journalElem("Title"),
       "ISOAbbreviation" = journalElem("ISOAbbreviation"))
}

# Retrieving all articles and information about journals
allArticles <- xpathSApply(xml, "//Article")
allArticles <- do.call(rbind.data.frame, 
                       lapply(allArticles, processArticleJournal))

articleColumns <- c("PMID", "Language", "ArticleTitle")
jIssueColumns <- c("Volume", "Issue", "PubYear", "PubMonth", "PubDay")
journalColumns <- c("ISSN", "ISSNType", "Title", "ISOAbbreviation")

# grouping by distinct journal issue
journalIssueGroups <- allArticles |> 
  dplyr::group_by(across(-all_of(articleColumns)))

# assigning journal issue foreign keys to article dataframe
articleDF <- allArticles |>
  dplyr::select(all_of(articleColumns)) |>
  dplyr::mutate(JournalIssueID = dplyr::group_indices(journalIssueGroups))

# retrieving information on all distinct journal issues and their journals
allJournalIssues <- journalIssueGroups |>
  dplyr::summarize(.groups = 'keep') |>
  dplyr::ungroup() |>
  dplyr::mutate(jiid = 1:n(), .before = Volume)

# Creating a group for each distinct Journal
journalGroups <- allJournalIssues |>
  dplyr::group_by(across(all_of(journalColumns)))

# assigning journal foreign keys to journalissue dataframe
journalIssueDF <- allJournalIssues |>
  dplyr::select(all_of(jIssueColumns)) |>
  dplyr::mutate(JournalID = dplyr::group_indices(journalGroups))

journalDF <- journalGroups |>
  dplyr::summarize(.groups = 'keep') |> 
  dplyr::ungroup() |> 
  dplyr::mutate(jid = 1:n(), .before = ISSN)

rm(allArticles, journalIssueGroups, allJournalIssues, journalGroups)

#' AUTHORS
#' 
#' Design Decisions:
#'  - for the sake of time, we chose to classify "unique" authors based on 
#'    all attributes, including lastName, ForeName, Initials, and ValidYN. This
#'    may cause entries to point to the same real life person although no
#'    duplicate tuples will be present in the database

# helper function that creates a list of attributes for an Author 
processAuthor <- function(author) {
  authorElem <- function(elemPath, type = "character") {
    getElem(author, elemPath, type)
  }

  list('PMID' = xpathSApply(author, "../../../@PMID")[[1]] |> 
         as("numeric"), 
       'ValidYN' = xpathSApply(author, "@ValidYN") == "Y", 
       'CollectiveName' = authorElem("CollectiveName"), 
       'LastName' = authorElem("LastName"), 
       'ForeName' = authorElem("ForeName"), 
       'Initials' = authorElem("Initials"), 
       'Suffix' = authorElem("Suffix"), 
       'Affiliation' = authorElem("Affiliation"))
}

allAuthors <- xpathSApply(xml, "//Author")
allAuthors <- do.call(rbind.data.frame, 
                      lapply(allAuthors, processAuthor))

# creating a group for each distinct author
authorGroups <- allAuthors |>
  dplyr::group_by(across(c(-PMID)))

#' Authorship table defined by PMIDs in the table and their corresponding
#' author group IDs
authorshipDF <- data.frame(aid = dplyr::group_indices(authorGroups),
                           PMID = allAuthors$PMID)

#' Author table defined by each unique author group
authorDF <- authorGroups |>
  summarize(.groups = 'keep') |> 
  ungroup() |>
  mutate(aid = 1:n(), .before = ValidYN)

rm(allAuthors, authorGroups)

# LOADING THE DATA INTO DATABASES

dbWriteTable(db, "Author", authorDF, append = TRUE)
dbWriteTable(db, "Journal", journalDF, append = TRUE)
dbWriteTable(db, "JournalIssue", journalIssueDF, append = TRUE)
dbWriteTable(db, "Article", articleDF, append = TRUE)
dbWriteTable(db, "Authorship", authorshipDF, append = TRUE)
dbDisconnect(db)
