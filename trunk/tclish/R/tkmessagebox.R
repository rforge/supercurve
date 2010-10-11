###
### TKMESSAGEBOX.R - Interface routines for tkMessageBox, a la Python
###

options(warn=1)
require(tcltk) || stop("tcltk support is missing")


##
## Private Constants
##

.icons <- list(ERROR="error",
               INFO="info",
               QUESTION="question",
               WARNING="warning")

.types <- list(ABORTRETRYIGNORE="abortretryignore",
               OK="ok",
               OKCANCEL="okcancel",
               RETRYCANCEL="retrycancel",
               YESNO="yesno",
               YESNOCANCEL="yesnocancel")

.responses <- list(ABORT="abort",
                   RETRY="retry",
                   IGNORE="ignore",
                   OK="ok",
                   CANCEL="cancel",
                   YES="yes",
                   NO="no")


##
## Public Methods
##

##-----------------------------------------------------------------------------
## Show an info message.
showinfo <- function(title,
                     message,
                     ...) {
    tkmessageBox(title=title,
                 message=message,
                 icon=.icons$INFO,
                 type=.types$OK,
                 ...)
}


##-----------------------------------------------------------------------------
## Show a warning message.
showwarning <- function(title,
                        message,
                        ...) {
    tkmessageBox(title=title,
                 message=message,
                 icon=.icons$WARNING,
                 type=.types$OK,
                 ...)
}


##-----------------------------------------------------------------------------
## Show an error message.
showerror <- function(title,
                      message,
                      ...) {
    tkmessageBox(title=title,
                 message=message,
                 icon=.icons$ERROR,
                 type=.types$OK,
                 ...)
}


##-----------------------------------------------------------------------------
## Ask a question. Returns response as character string.
askquestion <- function(title,
                        message,
                        ...) {
    dots <- list(...)
    if ('icon' %in% names(dots)) {
        if (dots['icon'] != .icons$QUESTION) {
            warning(sprintf("value of argument %s must be %s - overridden",
                            sQuote("icon"),
                            dQuote("question")))
        }
    }

    args <- c(list(type=.types$YESNO),
              dots,
              list(title=title,
                   message=message,
                   icon=.icons$QUESTION))
    response <- do.call("tkmessageBox", args)

    return(as.character(response))
}


##-----------------------------------------------------------------------------
## Ask if operation should proceed; return TRUE if the answer is ok.
askokcancel <- function(title,
                        message,
                        ...) {
    response <- tkmessageBox(title=title,
                             message=message,
                             icon=.icons$QUESTION,
                             type=.types$OKCANCEL,
                             ...)

    return(as.character(response) == .responses$OK)
}


##-----------------------------------------------------------------------------
## Ask if operation should be retried; return TRUE if the answer is yes.
askyesno <- function(title,
                     message,
                     ...) {
    response <- tkmessageBox(title=title,
                             message=message,
                             icon=.icons$QUESTION,
                             type=.types$YESNO,
                             ...)

    return(as.character(response) == .responses$YES)
}


##-----------------------------------------------------------------------------
## Ask if operation should be retried; return TRUE if the answer is retry.
askretrycancel <- function(title,
                        message,
                        ...) {
    response <- tkmessageBox(title=title,
                             message=message,
                             icon=.icons$WARNING,
                             type=.types$RETRYCANCEL,
                             ...)

    return(as.character(response) == .responses$RETRY)
}

