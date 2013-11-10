\name{collaborate}
\alias{collaborate}
\alias{edit.collaborativeDoc}
\alias{knit}
\alias{knit.collaborativeDoc}
\alias{load}
\alias{load.collaborativeDoc}
\alias{load.default}
\alias{source}
\alias{source.collaborativeDoc}
\alias{source.default}
\alias{summary.collaborativeDoc}
\title{Create and edit document collaboratively}
\usage{
  collaborate(name,
    project = getOption("collaborate")$project,
    group = getOption("collaborate")$group,
    localdoc = file.path(getwd(), paste0(name, ".Rmd")),
    buffermode = c("Markdown", "r"))

  edit.collaborativeDoc(name, where = c("web", "local"),
    ...)

  load(...)

  load.default(file, envir = parent.frame(),
    verbose = FALSE)

  load.collaborativeDoc(object, ...)

  source(...)

  source.default(file, local = FALSE, echo = verbose,
    print.eval = echo, verbose = getOption("verbose"),
    prompt.echo = getOption("prompt"),
    max.deparse.length = 150, chdir = FALSE,
    encoding = getOption("encoding"),
    continue.echo = getOption("continue"), skip.echo = 0,
    keep.source = getOption("keep.source"))

  source.collaborativeDoc(object, local = FALSE, ...)

  knit(object, ...)

  knit.collaborativeDoc(object, ...)

  summary.collaborativeDoc(object, ...)
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

  \item{name}{a collaborative editing object}

  \item{where}{either \code{"web"} or \code{"local"}
  indicating which version is to be edited}

  \item{\dots}{additional arguments -- currently ignored.}
}
\description{
  These objects and functions provide an R interface to
  Firepad collaborative documents.
}
\examples{
options(collaborate = list( group="mosaic-web", project="CS121") )
myProject <- collaborate(name="example000")
}

