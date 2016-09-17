\name{get_json}
\alias{get_json}
\title{
Request json at Guardian API
}
\description{
The function get_json makes a call at Guardian API to retrieve articles that match the criteria. The function is called by get_guardian and should not be used standalone.
}
\usage{
get_json(keywords, section, format, from.date, to.date, api.key)
}
\arguments{
  \item{keywords}{
Keyword to search Guardian API. Exemple: "Thatcher". For multiple keywords use "Margaret+Hilda+Thatcher".
}
  \item{section}{
Specifies news sections to narrow the query (articles from other news desks will be filtered out). It doesn not accept blank spaces, so use "word" if querying for articles in the "World news" section (and so on). It defaults to NULL in which case API search is performed across all sections.
}
  \item{format}{
Specifies format JSON or XML formar. It defaults to JSON.
}
  \item{from.date}{
Start date of search.
}
  \item{to.date}{
End date of search.
}
  \item{api.key}{
Guardian API-key.
}
}
\author{
Marco Bastos & Cornelius Puschmann
}
\keyword{ IO }
\keyword{ utilities }
