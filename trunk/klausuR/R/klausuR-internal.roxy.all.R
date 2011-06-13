# basic roxygenizing and packaging for all machines
#
## roxy.package() actions:
#		"roxy":			roxygenize the docs
#		"plugin": 		update rkward_klausuR
#		"package": 		build & install klausuR package
#		"doc",			update pdf documentation
#		"win":			update the windows binary package

## function roxy.description()
# internal file, to create package description
roxy.description <- function(val, version, date=Sys.Date()){

	## basic package info 
	pck.package.v 	<- "klausuR"
	if(identical(val, "package")){
		return(pck.package.v)
	} else {}
	pck.title.v 	<- "Multiple Choice Test Evaluation"
	if(identical(val, "title")){
		return(pck.title.v)
	} else {}
	pck.depds.v 	<- "R (>= 2.9.0),xtable,psychometric,methods,graphics,tools"
	if(identical(val, "depds")){
		return(pck.depds.v)
	} else {}

	# package description files
	pck.descr.v 	<- "A set of functions designed to quickly generate results of
							a multiple choice test. Generates detailed global results, lists
							for anonymous feedback and personalised result feedback (in LaTeX
							and/or PDF format), as well as item statistics like Cronbach's alpha or
							disciminatory power."
	pckg.dscrptn.v <- paste(
			"Package: ",pck.package.v,
			"\nType: Package",
			"\nTitle: ",pck.title.v,
			"\nVersion: ",version,
			"\nDate: ",date,
			"\nAuthor: m.eik michalke",
			"\nMaintainer: m.eik michalke <meik.michalke@uni-duesseldorf.de>",
			"\nDepends: ",pck.depds.v,
			"\nDescription: ",gsub("#'","\n",gsub("\n[[:space:]]*", " ", pck.descr.v)),
			"\nLicense: GPL (>= 3)",
			"\nEncoding: UTF-8",
			"\nLazyLoad: yes",
			"\nURL: http://r-forge.r-project.org/projects/klausur\n",
			sep="")
	if(identical(val, "description")){
		return(pckg.dscrptn.v)
	} else {}
	pckg.package.v <- paste(
			"#' ",pck.title.v,".\n#'\n#' \\tabular{ll}{",
			"\n#' Package: \\tab ",pck.package.v,"\\cr",
			"\n#' Type: \\tab Package\\cr",
			"\n#' Version: \\tab ",version,"\\cr",
			"\n#' Date: \\tab ",date,"\\cr",
			"\n#' Depends: \\tab ",pck.depds.v,"\\cr",
			"\n#' Encoding: \\tab UTF-8\\cr",
			"\n#' License: \\tab GPL (>= 3)\\cr",
			"\n#' LazyLoad: \\tab yes\\cr",
			"\n#' URL: \\tab http://r-forge.r-project.org/projects/klausur \\cr",
			"\n#' }\n#'",
			"\n#' ",gsub("\n#' #'","\n#'",gsub("\n[[:space:]]*", "\n#' ", pck.descr.v)),"\n#'",
			"\n#' @aliases ",pck.package.v,"-package ",pck.package.v,
			"\n#' @name ",pck.package.v,"-package",
			"\n#' @docType package",
			"\n#' @title ",pck.title.v,".",
			"\n#' @author m.eik michalke \\email{meik.michalke@@uni-duesseldorf.de}",
			"\n#' @keywords package",
			"\n#' @seealso \\code{\\link[klausuR:klausur]{klausur}}, \\code{\\link[klausuR:klausur.mufo]{klausur.mufo}},",
			"\n#'  \\code{\\link[klausuR:klausur.report]{klausur.report}},",
			"\n#'  \\code{\\link[klausuR:klausur.gen]{klausur.gen}}, \\code{\\link[klausuR:klausur.compare]{klausur.compare}},",
			"\n#'  \\code{\\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}, \\code{\\link[klausuR:klausur.gen.corr]{klausur.gen.corr}}",
			"\nroxygen <- function() NULL",
			"\nroxygen()\n",
			sep="")
	if(identical(val, "pckg.description")){
		return(pckg.package.v)
	} else {}
} ## end function roxy.description()


## function roxy.package()
roxy.package <- function(
	pck.package=roxy.description("package"),
	pck.title=roxy.description("title"),
	pck.version=pck.version.v,
	add.to.version="",
	pck.date=Sys.Date(),
	R.libs=R.libs.v,
	plugin.version=plugin.version.v,
	rkwd.plugins=rkwd.plugins.v,
	repo.rkwd=repo.rkwd.v,
	repo.root=repo.root.v,
	actions=c("roxy", "package"),
	local.r.dir=r.dir,
	local.roxy.dir=roxy.dir,
	local.pack.dir=pack.dir){

	old.dir <- getwd()
	setwd(local.pack.dir)
	on.exit(setwd(old.dir))

	pckg.dscrptn <- roxy.description("description", version=pck.version, date=pck.date)
	pckg.package <- roxy.description("pckg.description", version=pck.version, date=pck.date)

	repo.src.contrib <- file.path(repo.root, "src", "contrib")
	R.Version.win <- paste(R.Version()$major, ".", gsub("^([[:digit:]]+)(\\.)(.)", "\\1", R.Version()$minor, perl=TRUE), sep="")
	repo.win <- file.path(repo.root, "bin", "windows", "contrib", R.Version.win)

	# clean up
	unlink(list.files(local.r.dir, pattern=".*~$", full.names=TRUE))
	unlink(list.files(paste(local.r.dir,"/R",sep=""), pattern=".*~$", full.names=TRUE))
	unlink(list.files(paste(local.r.dir,"/inst/doc",sep=""), pattern=".*~$", full.names=TRUE))
	unlink(list.files(paste(local.r.dir,"/inst/doc",sep=""), pattern=".*backup$", full.names=TRUE))
		if("roxy" %in% actions){
			# re-write DESCRIPTION files
			cat(paste(pckg.dscrptn), file=file.path(local.r.dir, "DESCRIPTION"))
			cat(paste(pckg.package), file=file.path(local.r.dir, "R", paste(pck.package, "-package.R", sep="")))
			require(roxygen)
			roxygenize(local.r.dir, roxygen.dir=local.roxy.dir, use.Rd2=TRUE, unlink.target=TRUE)
		} else {}

	if("package" %in% actions){
		if("roxy" %in% actions){
		stopifnot(file.copy(Sys.glob(file.path(local.roxy.dir, "man", "*")), file.path(local.r.dir, "man"), overwrite=TRUE))
		message("build: Rd files copied from roxygen to man.")
		stopifnot(file.copy(file.path(local.roxy.dir, "NAMESPACE"), file.path(local.r.dir, "NAMESPACE"), overwrite=TRUE))
		message("build: NAMESPACE copied from roxygen to build dir.")
		} else {}

		## source-repo auffuellen
		jmp.back <- getwd()
		setwd(file.path(r.dir, ".."))
		repo.src.gz <- file.path(repo.src.contrib, paste(pck.package, "_", pck.version, ".tar.gz", sep=""))
		tar(repo.src.gz, files=pck.package,
			tar="/bin/tar",
			compression="gzip", extra_flags="-h --exclude=*~ --exclude-vcs")
		setwd(jmp.back)
		message(paste("repo: ", pck.package, "_", pck.version, ".tar.gz copied to src/contrib.",  sep=""))
		stopifnot(file.copy(file.path(R.libs, pck.package, "DESCRIPTION"), file.path(repo.src.contrib, "PACKAGES"), overwrite=TRUE))
		message("repo: src/contrib/PACKAGES (unix) updated.")
		install.packages(repo.src.gz, lib=R.libs, repos=NULL)
		message("build: package built and installed.")

		## changelog aktualisieren
		stopifnot(file.copy(file.path(local.r.dir, "ChangeLog"), file.path(repo.src.contrib, "ChangeLog"), overwrite=TRUE))
		message("repo: ChangeLog updated.")
	} else {}

	## pdf aktualisieren
	if("doc" %in% actions){
		# vignette erstellen und verschieben
		tools:::buildVignettes(dir=r.dir)
		pdf.vignette.src <- file.path(r.dir, "inst", "doc", paste(pck.package, "_vignette.pdf", sep=""))
		pdf.vignette.dst <- file.path(R.libs, "klausuR", "doc", paste(pck.package, "_vignette.pdf", sep=""))
		if(file.exists(pdf.vignette.src)){
			stopifnot(file.copy(pdf.vignette.src, pdf.vignette.dst, overwrite=TRUE))
			stopifnot(file.remove(pdf.vignette.src))
			message("build: PDF vignette created.")
		} else {}

		pdf.docs <- file.path(repo.root, paste(pck.package, ".pdf", sep=""))
		if(file.exists(pdf.docs)){
			stopifnot(file.remove(pdf.docs))
		} else {}
		r.cmd.doc.call <- paste("R CMD Rd2dvi --pdf --output=", pdf.docs, " --no-preview ", local.r.dir, " || exit 1", sep="")
		system(r.cmd.doc.call, intern=TRUE)
		message("build: PDF docs created.")
		## vignette kopieren
		pdf.vignette.repo <- file.path(repo.root, paste(pck.package, "_vignette.pdf", sep=""))
		if(file.exists(pdf.vignette.dst)){
			stopifnot(file.copy(pdf.vignette.dst, pdf.vignette.repo, overwrite=TRUE))
			message("repo: vignette updated.")
		} else {}
	} else {}

	## windows-repo auffuellen
	if("win" %in% actions){
		if(!file_test("-d", repo.win)){
			stopifnot(dir.create(repo.win, recursive=TRUE))
			message(paste("repo: created ", repo.win, ".", sep=""))
		} else {}
		win.package <- file.path(repo.win, paste(pck.package, "_", pck.version, ".zip", sep=""))
		if(file.exists(win.package)){
			stopifnot(file.remove(win.package))
		} else {}
		jmp.back <- getwd()
		setwd(R.libs)
		suppressWarnings(zip(win.package, pck.package))
		message(paste("repo: ", pck.package, "_", pck.version, ".zip (windows) created.", sep=""))
		setwd(jmp.back)
		stopifnot(file.copy(file.path(R.libs, pck.package, "DESCRIPTION"), file.path(repo.win, "PACKAGES"), overwrite=TRUE))
		message("repo: bin/PACKAGES (windows) updated.")
	} else {}

	if("plugin" %in% actions){
		jmp.back <- getwd()
		setwd(rkwd.plugins)
		RKWard.plugin.gz <- paste(pck.package, "_rkward-", plugin.version, ".tar.gz", sep="")
		if(file.exists(RKWard.plugin.gz)){
			stopifnot(file.remove(RKWard.plugin.gz))
		} else {}
		tar(RKWard.plugin.gz, files=paste(pck.package, "_rkward", sep=""),
			tar="/bin/tar",
			compression="gzip", extra_flags="-h --exclude=*~ --exclude-vcs")
		message(paste("rkward: plugin updated to", RKWard.plugin.gz))
		stopifnot(file.copy(RKWard.plugin.gz, repo.rkwd, overwrite=TRUE))
		message("rkward: plugin copied to repo.")
		setwd(jmp.back)
	} else {}

} ## end function roxy.package()
