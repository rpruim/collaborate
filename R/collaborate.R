#' Create and edit document collaboratively
#' 
#' These objects and functions provide an R interface to Firepad collaborative documents.
#' 
#' @rdname collaborate
#' @aliases collaborate
#' @param name a character string naming the document in the collaborative editor. 
#' It's best if this is simple and memorable, so you can communicate it to your collaborators. 
#' Spaces are not permitted.
#' @param localdoc a character string naming the (path to) a file on your R-system which will be 
#' synchronized to the collaborative buffer when using \code{load}.
#' By default, this is set to be the same as \code{name}. But you can use any document that you're 
#' willing to overwrite.  To browse for an existing file use \code{\link{file.choose}()} to select it. 
#' Dont' forget the file extension.
#' @param group a character string naming the Firepad group in which the document will be housed. 
#' @param project a character string naming the project. 
#' @baram uffermode a string, either \code{"Markdown"} or \code{"r"}.
#' (This may be extended in the future to include other allowable modes for the CodeMirror editor.
#' 
#' @export
#' @examples
#' options(collaborate = list( group="mosaic-web", project="CS121") )
#' myProject <- collaborate(name="example000")
#' 
collaborate <- function(
  name, 
  project = getOption("collaborate")$project,
  group=getOption("collaborate")$group,
  localdoc=file.path(getwd(),paste0(name,".Rmd")),
  buffermode=c("Markdown", "r")) {
  
  if (is.null(project) || is.null(group)) {
    stop("Missing 'project' and/or 'group' with no defaults.")
  }
  
  # Test whether localdoc already exists and ask if they want to override it.
  # To be implemented.
  
  buffermode <- match.arg(buffermode)
  
  if( !require(RCurl) ) stop("Must install 'RCurl' package.")
  if( !require(markdown) ) stop("Must install 'markdown' package.")
  if (missing(name))
    stop("Must specify name of collaborative buffer as the 'name' argument.")
  docURL <- paste('https://', group,
                  '.firebaseio.com/', project,'/',
                  name, '/first/.json?pretty=TRUE', sep='')
  editBufferURL <- paste("http://www.mosaic-web.org/go/firepad/examples/teamedit.html?project=",project,"&doc=",name,"&mode=",buffermode,sep="")
  synchronizeBufferURL <- paste("http://www.mosaic-web.org/go/firepad/examples/updateFirepad.html?project=",project,"&doc=",name,sep="")

  return( 
    structure( list( 
      project = project,
      group = group,
      buffermode = buffermode,
      localdoc = localdoc,
      docURL = docURL,
      editBufferURL = editBufferURL,
      synchronizeBufferURL = synchronizeBufferURL
    ), class="collaborativeDoc")
  )
}

#' @rdname collaborate
#' 
#' @param name a collaborative editing object
#' @param where either \code{"web"} or \code{"local"} indicating which version is to be edited
#' @param \dots additional arguments -- currently ignored.
#' @export
#' 
edit.collaborativeDoc <- function(name, where=c("web", "local"), ...) {
  where <- match.arg(where)
  switch(where,
         web =  browseURL(name$editBufferURL),
         local = { 
           cat(load(name), file=name$localdoc); 
           file.edit(name$localdoc)
         }
  )
}

#' @rdname collaborate
#' @export
#' 
load <- function(...) {
  UseMethod("load")
}

#' @rdname collaborate
#' @export
load.default <- base::load

#' @rdname collaborate
#' @export
load.collaborativeDoc <- function(object, ...) {
    content <- getURL(object$docURL)
    # Get rid of the opening and closing quotes
    content <- substr(content,2,nchar(content)-1)
    content <- gsub("\\\\n", "\n", content) # Should be restricted to being outside of quotes
    content <- gsub('\\\\','',content) # kill the escapes on the escaped quotes
    content
}

#' @rdname collaborate
#' @export
source <- function(...) {
  UseMethod("source")
}

#' @rdname collaborate
#' @export
source.default <- base::source

#' @rdname collaborate
#' @export
source.collaborativeDoc <- function(object, local=FALSE, ...) {
  content <- load(object)
  if (local) { 
    eval(parse(text=content), ...) 
  } else {
    eval(parse(text=purl(text=content)), envir=globalenv(), ...) 
  }
}

#' @rdname collaborate
#' @export
knit <- function(object, ...) {
  UseMethod("knit")
}

#' @rdname collaborate
#' @export
knit.collaborativeDoc <- function( object, ... ) { 
  tmpNames <- paste("team-edit-", object$project, "-", object$name, 
                     c(".html",".md",".Rmd"), sep="") 
  htmlName <- tmpNames[1]
  mdName <- tmpNames[2]
  rmdName <- tmpNames[3]
  writeLines(load(object), rmdName)
  knit2html( rmdName, output=mdName )
  markdownToHTML( mdName, output=htmlName )
  browseURL(htmlName)
  #rstudio::viewer(htmlName,height=500)
}

#' @rdname collaborate
#' @export
summary.collaborativeDoc <- function(object, ...) {
  return(
    list(project=object$project, 
         group=object$group, 
         doc=object$name, 
         localdoc=object$localdoc)
    )
}