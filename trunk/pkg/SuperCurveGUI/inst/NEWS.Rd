\name{NEWS}
\title{SuperCurveGUI News}
\encoding{UTF-8}

% Production release
\section{CHANGES IN VERSION 2.0.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Each analysis performed will be logged in its own logfile.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Workaround for issue where occasionally tabs would be missing
        from the user interface.
    }
    \itemize{
      \item Fixed minor issue with console diversions not shutting down
        after each analysis.
    }
  }
}

% Production release
\section{CHANGES IN VERSION 2.0.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Completely redesigned tabbed user interface.
    }
    \itemize{
      \item Settings from previous run can now be trivially
        reloaded, possibly tweaked, and rerun.
    }
    \itemize{
      \item Progress is monitored such that user can tell what
        stage the \pkg{SuperCurve} processing is currently at
        and what slide is being handled.
    }
  }
}

% Production release
\section{CHANGES IN VERSION 1.0.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added \code{sc()} as alias for \code{supercurveGUI()}
        for backwards compatibility.
    }
  }
}

% Production release
\section{CHANGES IN VERSION 1.0.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Creation of package so that Tcl/Tk requirement would not affect
        people who only wish to use package \pkg{SuperCurve} without the GUI.

      \item Renamed \code{sc()} as \code{supercurveGUI()}.

      \item Added filters to file selection dialogs.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Changed output directory selection dialog to not require an
        existing file, avoiding first-use "chicken & egg" scenario.

      \item Added appropriate dialog titles where necessary.
    }
  }
}

