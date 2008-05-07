###
### GUI.R
###

library(tcltk)

.browsePath <- function(title, initialdir = NULL) {
	if(!is.null(initialdir)) {
		fileName <- tclvalue(tkgetOpenFile(title=title, initialdir=initialdir)) 
	} else {
		fileName <- tclvalue(tkgetOpenFile(title=title)) 	
	}
	if (!nchar(fileName)) {
	    result <- NULL
	} else {
	    result <- dirname(fileName)
	}
	result
}

# Given a list of button names, create a dialog box with those labels
# Return a number indiciating which button in the list was selected
.listButtonDialog <- function(button.labels, title, msg) {
	# Create a new toplevel window
	tt <- tktoplevel()
	
	# Give the window a title
	tkwm.title(tt,title)
	tkgrid(tklabel(tt,text=msg))
	
	# Create a variable to keep track of the state of the dialog window:
	#   If the window is active,                                            done = 0
	#   If the window has been closed using the OK button,                  done = 1
	#   If the window has been closed using the Cancel button or destroyed, done = 2
	done <- tclVar(0)
	
	# Create buttons and for each one, set the value to the index number
	for(i in 1:length(button.labels)) {
		cmd <- substitute(function() tclvalue(done)<-res, list(res = i))
		tkgrid(tkbutton(tt, text=button.labels[i], command=eval(cmd)))
	}
	
	# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, assign 1 to done.
	tkbind(tt,"<Destroy>",function() tclvalue(done)<-1)
	
	tkfocus(tt)
	
	# Do not proceed with the following code until the variable done is non-zero.
	#   (But other processes can still run, i.e. the system is not frozen.)
	tkwait.variable(done)
	
	# The variable done is now non-zero, so we would like to record its value before
	# destroying the window tt.  If we destroy it first, then done will be set to 2
	# because of our earlier binding, but we want to determine whether the user pressed
	# OK (i.e. see whether done is equal to 1).
	
	doneVal <- as.integer(tclvalue(done))
	tkdestroy(tt)
	
	doneVal
}

.editBox <- function(title, msg, default = "", width = "40") {
	tt<-tktoplevel()
	tkwm.title(tt,msg)
	Text <- tclVar(default)
	done <- tclVar(0)
	entry.Text <-tkentry(tt,width="20",textvariable=Text)
	tkgrid(tklabel(tt,text=msg))
	tkgrid(entry.Text)
	
	OK.but <-tkbutton(tt,text="   OK   ",command=function() tclvalue(done) <- 1)
	tkgrid(OK.but)
	
	# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, assign 1 to done.
	tkbind(tt,"<Destroy>",function() tclvalue(done)<-1)
	
	tkfocus(tt)
	
	tkwait.variable(done)
	TextVal <- tclvalue(Text)
	tkdestroy(tt)
	TextVal
}

.trim <- function(str) {
	lt <- gsub('^[[:space:]]+', '', str)
	rt <- gsub('[[:space:]]+$', '', lt)
	rt	
}


# prompt user for parameters. run supercurve using specified directories
sc <- function() {
	require(tcltk)
	
    .path <- Sys.getenv("SC_DIR")
    if (nchar(.path) < 1) {
       .path <- NULL
    }
	
	if(is.null(.path)) {
		.path <- .browsePath("select .txt / quantification file directory")
	} else {
		.path <- .browsePath("select .txt / quantification file directory", .path)
	}
	
	if (!is.null(.path)) {
		Sys.putenv(SC_DIR=.path)
	}

	tiffdir <- .browsePath("select .tif / image directory", dirname(.path))
	results <- .browsePath("select output directory for results", dirname(.path))
	net.total <- .listButtonDialog(c("Mean Net", "Mean Total"), "Intensity Measure",  "Choose spot measure to use for quantification")
	if (net.total == 1) {
		measure <- "Mean.Net"
	} else {
		measure <- "Mean.Total"
	}
	
	curve.model <- .listButtonDialog(c("Monotone Increasing B-spline", "Loess", "Logistic"), "Dilution Curve Model",  "Choose a model for fitting the antibody response curve")
	if (curve.model == 1) {
		model <- "cobs"
	} else if (curve.model == 2){
		model <- "loess"
	} else {
		model <- "logistic"
	}
	
	controls <- .editBox("Controls", "Names of control spots", "control, pos con, neg con", width="60")
	controls <- unlist(strsplit(controls, ","))
	for(i in 1:length(controls)) {
		controls[i] <- .trim(controls[i])
	}
	
	settings <- paste(
	"txt dir ='", .path, "'\n",
	 "tiff dir ='", tiffdir,"'\n",
	 "result dir ='", results,"'\n",
	 "quantification type ='", measure, "'\n",
	 "dilution curve model ='", model, "'\n",
	 "control spot labels ='", paste(controls, collapse=", "),"'", sep =""
	)
	
	confirm <- tkmessageBox(message=paste("Run analysis with the following options:\n\n", settings, sep = ""),
	 type = "yesno"
	)
	
	if (as.character(confirm) == "no") {
		print("Quantification aborted")
		return()
	}
	
	version <- packageDescription("SuperCurve", fields="Version")
	
	cat("Run from gui. Settings:\n", settings, paste("\nSuperCurve Version = ", version), sep = "", file=file.path(results, "analysis.log"))
	      
	designparams <- RPPADesignParams(grouping="blockSample", center=FALSE, controls=list(controls))
	fitparams <- RPPAFitParams(measure = measure, ignoreNegative=FALSE, method='nlrob', warnLevel=-1, model=model)
	# rm(fitset)                       
	fitset <- RPPAFitDir(.path, designparams, fitparams)
	write.summary(fitset, file = 'supercurve', path = results, normalize = 'median', graphs=TRUE, tiffdir = tiffdir)
}

