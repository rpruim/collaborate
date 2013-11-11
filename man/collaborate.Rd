\name{collaborate}
\alias{collaborate}
\alias{edit.collaborativeDoc}
\alias{knit}
\alias{knit.collaborativeDoc}
\alias{load}
\alias{load.collaborativeDoc}
\alias{load.default}
\alias{print.collaborativeDoc}
\alias{source}
\alias{source.collaborativeDoc}
\alias{source.default}
\alias{summary.collaborativeDoc}
\title{Create and edit document collaboratively}
\usage{
  collaborate(name,
    localdoc = file.path(getwd(), paste0(name, ".Rmd")),
    buffermode = c("Markdown", "r"),
    project = getOption("collaborate")$project,
    group = getOption("collaborate")$group,
    firepad = getOption("collaborate")$firepad)

  \method{edit}{collaborativeDoc} (name,
    where = c("web", "local"), ...)

  load(object, ...)

  \method{load}{default} (object, ...)

  \method{load}{collaborativeDoc} (object, ...)

  source(...)

  \method{source}{default} (...)

  \method{source}{collaborativeDoc} (object, local = FALSE,
    ...)

  knit(object, ...)

  \method{knit}{collaborativeDoc} (object, ...)

  \method{summary}{collaborativeDoc} (object, ...)

  \method{print}{collaborativeDoc} (x, ...)
}
\arguments{
  \item{name}{a character string naming the document in the
  collaborative editor. It's best if this is simple and
  memorable, so you can communicate it to your
  collaborators. Spaces are not permitted.}

  \item{localdoc}{a character string naming the (path to) a
  file on your R-system which will be synchronized to the
  collaborative buffer when using \code{load}. By default,
  this is set to be the same as \code{name}. But you can
  use any document that you're willing to overwrite.  To
  browse for an existing file use
  \code{\link{file.choose}()} to select it. Dont' forget
  the file extension.}

  \item{group}{a character string naming the Firepad group
  in which the document will be housed.}

  \item{project}{a character string naming the project.}

  \item{buffermode}{a string, either \code{"Markdown"} or
  \code{"r"}. (This may be extended in the future to
  include other allowable modes for the CodeMirror
  editor.)}

  \item{where}{either \code{"web"} or \code{"local"}
  indicating which version is to be edited}

  \item{firepad}{a character string specifying the base URL
  for the firepad server}

  \item{object}{an object}

  \item{x}{and object}

  \item{local}{a logical}

  \item{\dots}{additional arguments}
}
\description{
  These objects and functions provide an R interface to
  Firepad collaborative documents.
}
\examples{
options(collaborate = list( firepad="http://www.mosaic-web.org/go", group="mosaic-web", project="CS121") )
myProject <- collaborate(name="example000")
summary(myProject)
}

