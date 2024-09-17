## Dummy R code for stupid R check
os = Sys.info()['sysname']
pdftex = dirname(Sys.which("pdflatex"))
## Windows attempt
if (os %in% c("Windows")) {
	if (pdftex=="") {
		pdftex = "C:\\Program Files\\MiKTeX\\miktex\\bin\\x64"
		found = dir.exists(pdftex)
		if (found) {
			onpath = pdftex %in% strsplit(Sys.getenv()[["PATH"]], split=";")[[1]]
			if (!onpath)
				Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=";"))
		} else {
			stop("Cannot find 'pdflatex' on this system; install 'MiKTex' and try again.")
		}
	} else {
		pdftex = gsub("/","\\\\",pdftex)
		target = sub("\\\\$", "", strsplit(Sys.getenv()[["PATH"]], split=";")[[1]])  ## this is getting ridiculous
		onpath = pdftex %in% target
		if (!onpath)
			Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=";"))
	}
}
## Linux attempt
if (os %in% c("Linux")) {
	if (pdftex=="") {
		pdftex = "/usr/local/texlive/*/bin/x86_64-linux"
		found = dir.exists(pdftex)
		if (found) {
			onpath = pdftex %in% strsplit(Sys.getenv()[["PATH"]], split=":")[[1]]
			if (!onpath)
				Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=":"))
		} else {
			stop("Cannot find 'pdflatex' on this system; install 'texlive-latex-base' and try again.")
		}
	} else {
		onpath = pdftex %in% strsplit(Sys.getenv()[["PATH"]], split=":")[[1]]
		if (!onpath)
			Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=":"))
	}
}
## MacOS attempt
if (os %in% c("Darwin")) {
	if (pdftex=="") {
		pdftex = "/usr/texbin"
		found = dir.exists(pdftex)
		if (found) {
			onpath = pdftex %in% strsplit(Sys.getenv()[["PATH"]], split=":")[[1]]
			if (!onpath)
				Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=":"))
		} else {
			stop("Cannot find 'pdflatex' on this system; install 'texlive' and try again.")
		}
	} else {
		onpath = pdftex %in% strsplit(Sys.getenv()[["PATH"]], split=":")[[1]]
		if (!onpath)
			Sys.setenv(PATH=paste(Sys.getenv("PATH"), pdftex, sep=";"))
	}
}
