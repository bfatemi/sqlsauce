#' Github Authentication
#'
#' Functions to enable backend github authentication
#'
#' @name BackendAuth
NULL
# authenv <- new.env(parent = emptyenv())


#' @describeIn BackendAuth returns github app key
GitApp <- function(){
    app_name <- "sqlsauce"
    cid <- "767908ae5ba9cafa38c6"
    ckey <- "fc7ba02b5b195f6e47729299645c6af1a94aec39"
    httr::oauth_app(app_name, cid, ckey)
}


#' @describeIn BackendAuth A basic function to interact with Github API.
#'              Proper credits to Hadley Wickham coming.
#' @param path The API path
#' @param gtoken Additional headers for API call
#' @param json A boolean to indicate whether to check response for json format. Default = TRUE
github_api <- function(path, gtoken=NULL, json=TRUE) {

    url <- httr::modify_url("https://api.github.com", path = path)

    if(!interactive()){
        url <- httr::modify_url("https://api.github.com", path = path)
        resp <-httr::GET(url, httr::authenticate("bfatemi", Sys.getenv("GITHUB_PAT")))
    }else{
        resp <-httr::GET(url, gtoken)
    }



    if(httr::http_type(resp) != "application/json") {
        # warning("API did not return json", call. = FALSE)
        return(resp)
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    if (httr::http_error(resp)) {
        stop(
            sprintf(
                "GitHub API request failed [%s]\n%s\n<%s>",
                httr::status_code(resp),
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


#' @describeIn BackendAuth A function that checks whether the authenticated github user
#'              is a member of the given organization
#' @param orgname Character vector of length 1 that represents the name of the organization
#' @export
CheckOrg <- function(orgname="intusurg"){

    # if not interactive, like testing suite, then pull the locally stored token, else do oauth dance
    if(!interactive()){
        r1 <- github_api("/user")
    }else{
        token <- httr::oauth2.0_token(endpoint = httr::oauth_endpoints("github"),
                                      app = GitApp(),
                                      scope = "read:org, user:email, write:public_key",
                                      cache = TRUE)
        gtoken <- httr::config(token = token)
        r1 <- github_api("/user", gtoken)
    }

    usrinfo <- jsonlite::fromJSON(httr::content(r1$response, "text"), simplifyVector = FALSE)
    usr <- usrinfo$login

    # Get authenticated users organization and tmp check if its intusurg, but later
    # change to just check github username in an encrypted list of allowed users
    path <- paste0("/orgs/", orgname, "/members/", usr)
    r2 <- suppressWarnings(github_api(path, gtoken)) # only care about code that is being returned

    if(r2$status_code == 204) return(TRUE)
    return(FALSE)
}


#' @describeIn BackendAuth An updated version of the prior function to pull access info.
#'          This function pulls access info from remote location and stores it locally.
#'          It can also update local file
#' @param bUpdate A boolean representing whether to update the local access file
#' @export
Databases <- function(bUpdate = FALSE){
    # soon update this function to change reading/updating local package file to
    # reading updating global R file but using a environment var. Advantages?
    packageDir <- system.file("R", package = "sqlsauce")

    destn <- suppressWarnings(normalizePath(paste0(packageDir, "\\dbdata\\database")))

    # check if this should init data
    if(!file.exists(destn)) bUpdate <- TRUE

    # Update db file if user asks or if we need to initialize it (set true above)
    if(bUpdate){

        # remove hidden cached file
        cache_path <- paste0(packageDir, "\\.httr-oauth")
        suppressWarnings(file.remove(cache_path))

        # Since we need to pull remote data and update local file,
        # Check access for remote data using CheckOrg
        bHasISI <- CheckOrg()

        if(bHasISI){
            # Pull the data and save as temp file. Then replace old file safely with FileMove
            tmpdestn <- suppressWarnings(tempfile(tmpdir = normalizePath(dirname(destn))))

            if(!interactive()){
                # only accept raw
                httr::set_config(httr::add_headers(Accept = "application/vnd.github.raw"), override = FALSE)

                # get private data on private repo using token
                resp <- github_api("/repos/bfatemi/sqlsauce_auth/contents/data/serialdat")


                # set back to json
                httr::set_config(httr::add_headers(Accept = "application/json"), override = FALSE)

            }else{
                token <- httr::oauth2.0_token(endpoint = httr::oauth_endpoints("github"),
                                              app = GitApp(),
                                              scope = "read:org, user:email, write:public_key",
                                              cache = TRUE)
                gtoken <- httr::config(token = token)

                # only accept raw
                httr::set_config(httr::add_headers(Accept = "application/vnd.github.raw"), override = FALSE)

                resp <- github_api("/repos/bfatemi/sqlsauce_auth/contents/data/serialdat",
                                   gtoken = token, json = FALSE)

                # set back to json
                httr::set_config(httr::add_headers(Accept = "application/json"), override = FALSE)
            }
            dat <- rawToChar(resp$content)


            # save extracted data in a tmp file in a temp location,
            # then copy to temp file within the package home folder
            tmploc <- tempfile()
            writeLines(dat, con = tmploc)
            FileMove(fpath = tmploc, destn = tmpdestn) # move temp file into package folder
            bMove <- FileMove(fpath = tmpdestn, destn = destn, overwrite = TRUE) # replace the old file with the temp file
            if(!bMove)
                warning("File not updated. Old data returned")
        }else{
            stop("No access to database file")
        }
    }
    return(readRDS(destn))
}

# print.github_api <- function(x, ...) {
#     cat("<GitHub ", x$path, ">\n", sep = "")
#     str(x$content)
#     invisible(x)
# }

#' @describeIn BackendAuth A better function to move files. Move later
#' @param fpath File to move
#' @param destn new location to move to
#' @param overwrite A boolean indicating whether to overwrite existing file at destination
#' @param makedir A boolean indicating whether to create a new directory for destination file
#'      if one did not previously exist (default = TRUE)
FileMove <- function(fpath=NULL, destn=NULL, overwrite=FALSE, makedir = TRUE){

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


#' @describeIn BackendAuth A function to retrieve the locally stored access token
github_pat <- function() {
    pat <- Sys.getenv('GITHUB_PAT')
    if (identical(pat, "")) {
        stop("Please set env var GITHUB_PAT to your github personal access token",
             call. = FALSE)
    }

    pat
}

# #' @describeIn BackendAuth A function to authorize app through github
# #' @param reauth A boolean to indicating whether to reauthorize access (default = FALSE)
# #' @export
# deprecatGitAuth <- function(cache = TRUE){
#
#     # if(exists("gtoken", envir = authenv) && !reauth)
#     #     return(authenv$gtoken)
#
#     # if token already exists as environment variable
#     pat <- Sys.getenv("GITHUB_PAT", "")
#     # token is 396aa4dafc93b5de09d37113c5f0bb7d08adc3f5
#     if (!identical(pat, "")) {
#         gtoken <- httr::add_headers(Authorization = paste0("token ", pat))
#     }else if (!interactive()) {
#         stop("In non-interactive environments, please set GITHUB_PAT env to a GitHub",
#              " access token (https://help.github.com/articles/creating-an-access-token-for-command-line-use)",
#              call. = FALSE)
#     }
#
#     app <- GitApp()
#     endpt <- httr::oauth_endpoints("github")
#     access <- "read:org, user:email, write:public_key"
#     token <- httr::oauth2.0_token(endpt, app, scope = access, cache = cache)
#     # gtoken <- httr::config(token = token)
#     # assign("gtoken", gtoken, envir = authenv)
#     return(token)
# }


# DigitalOceanAPI <- function(...){
#     # Make this a structure as it will hold family of functions
#
#     # Idea is to create functions that parallelize common tasks at the vector level,
#     # then feed into a parallelization at the list level.
#     #
#     # For example, we can parallize running the function mean on multiple vectors,
#     # but what if each vector was also too large and required it's each distributed
#     # processing of the average?
#
#
#
#     # Uses Roboust R Client for DO API ----------------------------------------
#
#     # This function should be able to parallize at the local level if possible, and/or
#     # talk to DIGITALOCEAN API and serialize + send data over to an existing hadoop
#     # cluster. A seperate function will be created to spawn a new cluster with digitalocean
#     #
#     # Quick Description: An interface to hadoop that abstracts away any cluster installation
#     # or sys administration required, using the digitalocean api, for easy and quick
#     # distributed computing for common tasks on big data
#     #
#     # Needs: make it possible to spawn a new cluster all through an R function call, and
#     # then also make it easy to send a computation to the active node. This requires an
#     # updated (and possibly new) R hadoop interface, and creating a previously non-existent
#     # R client for digitalocean. User would have to have a digitalocean account, but spawning
#     # new clusters, monitoring computations, expanding, shutting down, tracking costs, will
#     # all be wrapped in user-friendly R function calls
#     #
#     # Applications:
#     # Creating models or algorithms that dynamically add nodes based on
#     # set conditions or in the case of accuracy or desired outcome potentially being affected by
#     # the size of a batch of data being processed (or even total execution time), apply unsupervised
#     # learning for a smart algorithm optimizes parameters of distributed computing to acheive
#     # better results. For example: if I need something done faster, and I'm willing to accept
#     # a higher cost, I can specify my ideal cost per minute, and the maximum minutes I'm willing to
#     # to wait until aborting. The algo would log meta data about it's how long it takes to process
#     # certain types of information and amount of information, and uses that in addition to users limits
#     # to (1) not execute if not possible to meet user's limits, or (2) warn the user that it was X
#     # pcnt likely that it would hit the ceiling but average processing time per minute would be
#     # Y more than desired.
#
#     # In the case of finance, if the algo believes that speed of processing and executing trades
#     # changes with certain finance events (one example is qtrly financial data dumps occuring for a
#     # set of frequently interchanging companies, or that learning whether frequency trades impacts
#     # the performance of a strategy, it has the power to dynamically adjust size of cluster
#     # and even optimize marginal increases in returns with processing power costs
#
#     # look up what the fastest mean function in R or Python is, and benchmark against
#     # base R and this function
# }
#
#
#
#
#
# #
# # github_api("/repos/bfatemi/sqlsauceauth/readme", auth)
# #
# # ll_all <- readRDS(destn)



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

