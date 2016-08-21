authenv <- new.env()

GitApp <- function(){
    app_name <- "sqlsauce"
    cid <- "767908ae5ba9cafa38c6"
    ckey <- "2155d57dacf121f4dc940bc33375afd076cdf8c3"
    httr::oauth_app(app_name, cid, ckey)
}

GitAuth <- function(reauth = FALSE){

    if (exists("auth_config", envir = authenv) && !reauth)
        return(authenv$auth_config)

    # if token already exists as environment variable
    pat <- Sys.getenv("GITHUB_PAT", "")
    if (!identical(pat, "")) {
        auth_config <- httr::add_headers(Authorization = paste0("token ", pat))
    }
    else if (!interactive()) {
        stop("In non-interactive environments, please set GITHUB_PAT env to a GitHub",
             " access token (https://help.github.com/articles/creating-an-access-token-for-command-line-use)",
             call. = FALSE)
    }
    else {
        app <- GitApp()
        endpt <- httr::oauth_endpoints("github")
        access <- "write:public_key, read:org"

        token <- httr::oauth2.0_token(endpt, app, scope = access, cache = !reauth, use_basic_auth = TRUE)

        auth_config <- httr::config(token = token)
    }
    assign("auth_config", auth_config, envir = authenv)
    return(auth_config)
}


github_api <- function(path, config=NULL, json=TRUE) {
    url <- modify_url("https://api.github.com", path = path)
    resp <- GET(url, config)

    if (http_type(resp) != "application/json") {
        warning("API did not return json", call. = FALSE)
        return(resp)
    }
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
    if (http_error(resp)) {
        stop(
            sprintf(
                "GitHub API request failed [%s]\n%s\n<%s>",
                status_code(resp),
                parsed$message,
                parsed$documentation_url
            ),
            call. = FALSE
        )
    }
    structure(
        list(
            content = parsed,
            path = path,
            response = resp
        ),
        class = "github_api"
    )
}



CheckOrg <- function(){

    # get current authenticated user, or authenticate
    auth <- GitAuth()

    r1 <- github_api("/user", auth)
    usrinfo <- jsonlite::fromJSON(content(r1$response, "text"), simplifyVector = FALSE)
    usr <- usrinfo$login

    # Get authenticated users organization and tmp check if its intusurg, but later
    # change to just check github username in an encrypted list of allowed users
    path <- paste0("/orgs/intusurg/members/", usr)

    r2 <- suppressWarnings(github_api(path, auth))

    if(r2$status_code == 204) return(TRUE)
    return(FALSE)
}


Databases <- function(bUpdate = FALSE){

    # soon update this function to change reading/updating local package file to
    # reading updating global R file but using a environment var. Advantages?
    #
    packageDir <- normalizePath(path.package("sqlsauce", quiet = FALSE))
    destn <- suppressWarnings(normalizePath(paste0(packageDir, "\\dbdata\\database")))

    # file definitely exists, so unless we need to update it, return data
    if(bUpdate){
        # Since we need to pull remote data and update local file,
        # Check access for remote data using CheckOrg
        bHasISI <- CheckOrg()

        if(bHasISI){
            # Pull the data and save as temp file. Then replace old file safely with FileMove
            tmpdestn <- suppressWarnings(tempfile(tmpdir = normalizePath(dirname(destn))))

            # get token
            auth <- GitAuth()
            headr <- add_headers(Accept = "application/vnd.github.raw")
            auth2 <- httr::set_config(config = headr)

            # get private data on private repo using token
            filepath <- "/data/serialdat"
            urlcall <- "/repos/bfatemi/sqlsauce_auth/contents"

            path <- paste0(urlcall, filepath)
            r1 <- suppressWarnings(github_api(path, auth, json = FALSE))
            dat <- rawToChar(r1$content)

            # save extracted data in a tmp file in a temp location,
            # then copy to temp file within the package home folder
            tmploc <- tempfile()
            writeLines(dat, con = tmploc)
            FileMove(destn = tmpdestn, fpath = tmploc) # move temp file into package folder
            bMove <- FileMove(destn, tmpdestn, overwrite = TRUE) # replace the old file with the temp file
            if(!bMove)
                warning("File not updated. Old data returned")
        }else{
            stop("No access to database file")
        }
    }
    return(readRDS(destn))
}

print.github_api <- function(x, ...) {
    cat("<GitHub ", x$path, ">\n", sep = "")
    str(x$content)
    invisible(x)
}


FileMove <- function(destn=NULL, fpath=NULL, overwrite=FALSE, makedir = TRUE){

    # automate common helpful checks that are done before a file is moved
    # wrapper around file.rename
    if(is.null(destn) | is.null(fpath)){
        warning("Not moved. Either destn or fpath not provided in arguments")
        return(FALSE)
    }

    # normalize everything
    destn <- suppressWarnings(normalizePath(destn))
    fpath <- suppressWarnings(normalizePath(fpath))
    fpathDir <- suppressWarnings(normalizePath(dirname(fpath)))
    destnDir <- suppressWarnings(normalizePath(dirname(destn)))

    # check target for existing file
    if(!file.exists(fpath)){
        warning("No file to move exists. Check argument fpath")
        return(FALSE)
    }

    # check destn for existing file and user given overwrite flag
    if(file.exists(destn)){
        if(!overwrite){
            warning("Did not move. Set overwrite = TRUE to overwrite file at destn")
            return(FALSE)
        }

        # get ext to create temp file but make sure there are no dots in file
        tmp <- stringr::str_split_fixed(basename(fpath), "\\.", n=2)
        if(ncol(tmp) > 2)
            stop("invalid filename. contains dots")

        base <- tmp[[2]]
        tmpfpath <- tempfile(tmpdir = fpathDir)

        if(base != ""){
            tmpfpath <- paste(tmpfpath, base, sep = ".")

            bCopied <- file.copy(fpath, tmpfpath) # should never error
            if(!bCopied){
                warning("error with copy during safe replacement of destn. No changes made")
                suppressWarnings(file.remove(tmpfpath))
                return(FALSE)
            }
            bRemoved <- file.remove(fpath)
            if(!bRemoved){
                warning("problem removing old file. Changes reverted")
                file.remove(tmpfpath)
                return(FALSE)
            }
            return(file.rename(tmpfpath, fpath))
        }
    }

    # file does not exist, but check existence of directory
    # Create if it does not exist (unless usr set makedir=FALSE)
    if(!isTRUE(file.info(destnDir)$isdir)){
        if(!makedir){
            warning("Destination directory does not exist and makedir set to FALSE")
            return(FALSE)
        }
        dir.create(destnDir, recursive=TRUE)
    }
    return(file.rename(fpath, destn))
}



DigitalOceanAPI <- function(...){
    # Make this a structure as it will hold family of functions

    # Idea is to create functions that parallelize common tasks at the vector level,
    # then feed into a parallelization at the list level.
    #
    # For example, we can parallize running the function mean on multiple vectors,
    # but what if each vector was also too large and required it's each distributed
    # processing of the average?



    # Uses Roboust R Client for DO API ----------------------------------------

    # This function should be able to parallize at the local level if possible, and/or
    # talk to DIGITALOCEAN API and serialize + send data over to an existing hadoop
    # cluster. A seperate function will be created to spawn a new cluster with digitalocean
    #
    # Quick Description: An interface to hadoop that abstracts away any cluster installation
    # or sys administration required, using the digitalocean api, for easy and quick
    # distributed computing for common tasks on big data
    #
    # Needs: make it possible to spawn a new cluster all through an R function call, and
    # then also make it easy to send a computation to the active node. This requires an
    # updated (and possibly new) R hadoop interface, and creating a previously non-existent
    # R client for digitalocean. User would have to have a digitalocean account, but spawning
    # new clusters, monitoring computations, expanding, shutting down, tracking costs, will
    # all be wrapped in user-friendly R function calls
    #
    # Applications:
    # Creating models or algorithms that dynamically add nodes based on
    # set conditions or in the case of accuracy or desired outcome potentially being affected by
    # the size of a batch of data being processed (or even total execution time), apply unsupervised
    # learning for a smart algorithm optimizes parameters of distributed computing to acheive
    # better results. For example: if I need something done faster, and I'm willing to accept
    # a higher cost, I can specify my ideal cost per minute, and the maximum minutes I'm willing to
    # to wait until aborting. The algo would log meta data about it's how long it takes to process
    # certain types of information and amount of information, and uses that in addition to users limits
    # to (1) not execute if not possible to meet user's limits, or (2) warn the user that it was X
    # pcnt likely that it would hit the ceiling but average processing time per minute would be
    # Y more than desired.

    # In the case of finance, if the algo believes that speed of processing and executing trades
    # changes with certain finance events (one example is qtrly financial data dumps occuring for a
    # set of frequently interchanging companies, or that learning whether frequency trades impacts
    # the performance of a strategy, it has the power to dynamically adjust size of cluster
    # and even optimize marginal increases in returns with processing power costs

    # look up what the fastest mean function in R or Python is, and benchmark against
    # base R and this function
}





#
# github_api("/repos/bfatemi/sqlsauceauth/readme", auth)
#
# ll_all <- readRDS(destn)



#     Databases <- readRDS(destn)
#
#     # dat <- jsonlite::fromJSON(content(r1$response, "text"))
#     # rawToChar(r1$content, multiple = TRUE)
#
#     # make destination path of data, check if file exists, clean replace if so
#     rawToChar(base64(dat$content, FALSE, "raw"))
#     cat(rawToChar(content(r1$response, as = "raw")))
#     auth$auth_token$app$key
#     writeLines(text = dat$content)), encode = FALSE, mode = "raw")
#
#     readRDS(writeLines(text = dat$content))
#
#     readBin(dat$content, what = "raw")
#
#     readRDS(dat$content)
#
#     dat$content
#
#     readRDS(dat$content)
#
# }

