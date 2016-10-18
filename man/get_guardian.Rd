\name{get_guardian}
\alias{get_guardian}
\title{
Search Guardian API for news articles that match the criteria
}
\description{
The function get_guardian takes four variables (keyword(s), starting date, end date, and API-key) and returns a data frame with 27 columns, with the last column including the full text of the article.

Search criteria accepts single or multiple keywords concatenated with "+" (e.g. "keyword1+keyword2"). It also accepts Boolean queries with and/or/not between words to refine searches. For exact phrases and matches, please encapsulate the keywords in \%22 (e.g "\%22Death+of+Margaret+Thatcher\%22").

From version 0.5 onwards, the function get_guardian returns the full text of articles and requires a Guardian API-key. Guardian API-key can be obtained by registering at <http://open-platform.theguardian.com/access/>.
}
\usage{
get_guardian(keywords, section, format, from.date, to.date, api.key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keywords}{
Keyword to search Guardian API. Example: "Thatcher". For multiple keywords use "Margaret+Hilda+Thatcher".
}
  \item{section}{
Specifies news sections to narrow the query (articles from other news desks will be filtered out). It doesn not accept blank spaces, so use "world" if querying for articles in the section "world news" (and so on). It defaults to NULL in which case the API request is performed across all sections. For referece, these are some of the most common news sections:

entertainment

sport

world

opinion

economy

national

artanddesign

politics

environment

lifestyle

jobs

education

technology

science

fashion

culture

society

tourism

health

cars

uk-news

business

football

stage

money

commentisfree
}
  \item{format}{
Specifies JSON or XML format. It defaults to JSON.
}
  \item{from.date}{
Start date of search.
}
  \item{to.date}{
End date of search.
}
  \item{api.key}{
A Guardian API-key is necessary to retrieve the full text of news articles. A Guardian API-key can be obtained by registering at <http://open-platform.theguardian.com/access/>.
}
}
\value{
Returns a data frame with 27 variables. Rows refer to news articles and columns to the news item's data.
}
\references{
Information about the Content API is available on Guardian Item Reference Guide (http://open-platform.theguardian.com/documentation/).
}
\author{
Marco Bastos & Cornelius Puschmann
}
\examples{
## Not run:
results <- get_guardian("islamic+state", 
	section="world",
	from.date="2014-09-16", 
	to.date="2014-09-16", 
	api.key="3xzg2fk53jcdgaj5tbwqqhcz")
## End(Not run)
}
\keyword{ IO }
\keyword{ utilities }
