#  These are modified versions of piggyback, where github enterprise is allowed.



parse_repo2 <- function(repo) {
  r <- strsplit(repo, "/")[[1]]
  if (length(r) != 2) {
    cli::cli_abort(c("Could not parse {.val {repo}} as a GitHub repository.","Make sure you have used the format: {.val owner/repo}"))
  }
  return(r)
}


# used by pb_upload_file2, pb_upload2
pb_releases2 <- function(repo = paste(gh::gh_tree_remote(), collapse = "/"),
                         .api_url = gh:::default_api_url(),
                         .token = gh::gh_token(.api_url),
                         verbose = FALSE) {
  r <- parse_repo2(repo)
  releases <- tryCatch(
    gh::gh("/repos/:owner/:repo/releases",owner = r[[1]], repo = r[[2]],
           .limit = Inf,
           .api_url = .api_url,
           .token = .token),
    error = function(cnd) {
      cli::cli_abort(c(`!` = "Cannot access release data for repo {.val {repo}}.","Check that you have provided a {.code .token} and that the repo is correctly specified.",unlist(strsplit(cnd$message, "\\n"))))
    }
  )
  if (length(releases) == 0) {
    if (verbose) {
      cli::cli_warn(c(`!` = "No GitHub releases found for {.val {repo}}!","You can make a new one with {.fun piggyback::pb_new_release}"))
    }
    return(invisible(data.frame()))
  }
  out <- data.frame(release_name = vapply(releases, `[[`, character(1),"name"),
                    release_id = vapply(releases, `[[`, integer(1),"id"),
                    release_body = vapply(releases, `[[`, character(1),"body"),
                    tag_name = vapply(releases, `[[`, character(1),"tag_name"),
                    draft = vapply(releases, `[[`, logical(1),"draft"),
                    created_at = vapply(releases, `[[`, character(1),"created_at"),
                    published_at = vapply(releases, `[[`,character(1), "published_at"),
                    html_url = vapply(releases,`[[`, character(1), "html_url"),
                    upload_url = vapply(releases,`[[`, character(1), "upload_url"),
                    n_assets = vapply(releases,function(x) length(x[["assets"]]), integer(1)))
  return(out)
}

# used by pb_upload_file2
# uses get_release_assets2
pb_info2 <- function (repo = paste(gh::gh_tree_remote(), collapse = "/"),
                      tag = NULL,
                      .api_url = gh:::default_api_url(),
                      .token = gh::gh_token(.api_url)) {
  r <- parse_repo2(repo)
  releases <- pb_releases2(repo = repo, .token = .token,
                           .api_url = .api_url,
                           verbose = FALSE)
  if (nrow(releases) == 0) {
    return(data.frame(file_name = "", size = 0L, timestamp = lubridate::as_datetime(0),
                      tag = "", owner = r[[1]], repo = r[[2]], upload_url = "",
                      browser_download_url = "", id = "", state = "", stringsAsFactors = FALSE))
  }
  if (length(tag) == 1 && tag == "latest" && !"latest" %in% releases$tag_name)
    tag <- releases$tag_name[[1]]
  if (!is.null(tag))
    releases <- releases[releases$tag_name %in% tag, ]
  info <- get_release_assets2(releases = releases, r = r,
                              .api_url=.api_url,
                              .token = .token)
  return(info)
}


# uses pb_info2, pb_releases2
pb_upload_file2 <- function(file,
                            repo = paste(gh::gh_tree_remote(), collapse = "/"),
                            tag = "latest", name = NULL,
                            overwrite = "use_timestamps",
                            use_timestamps = NULL,
                            show_progress = TRUE,
                            .api_url = gh:::default_api_url(),
                            .token = gh::gh_token(.api_url),
                            dir = NULL) {
  file_path <- do.call(file.path, compact(list(dir, file)))
  if (!file.exists(file_path)) {
    cli::cli_warn("File {.file {file_path}} does not exist.")
    return(NULL)
  }
  if (!is.null(use_timestamps)) {
    cli::cli_warn("{.code use_timestamps} argument is deprecated, please set {.code overwrite = 'use_timestamps'} instead")
  }
  use_timestamps <- switch(as.character(overwrite),
                           `TRUE` = FALSE,
                           `FALSE` = FALSE,
                           use_timestamps = TRUE
  )
  overwrite <- switch(as.character(overwrite),
                      `TRUE` = TRUE,
                      `FALSE` = FALSE,
                      use_timestamps = TRUE
  )
  progress <- httr::progress("up")
  if (!show_progress) {
    progress <- NULL
  }
  if (is.null(name)) {
    name <- basename(file_path)
  }
  df <- pb_info2(repo = repo, tag = tag,
                 .api_url = .api_url,
                 .token = .token)
  i <- which(df$file_name == name)
  if (length(i) > 0) {
    if (use_timestamps) {
      local_timestamp <- fs::file_info(file_path)$modification_time
      no_update <- local_timestamp <= df[i, "timestamp"]
      if (no_update) {
        cli::cli_warn("Matching or more recent version of {.file {file_path}} found on GH, not uploading.")
        return(invisible(NULL))
      }
    }
    if (overwrite) {
      gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
             owner = df$owner[[1]], repo = df$repo[[1]], id = df$id[i],
             .api_url = .api_url,
             .token = .token)
    } else {
      cli::cli_warn("Skipping upload of {.file {df$file_name[i]}} as file exists on GitHub and {.code overwrite = FALSE}")
      return(invisible(NULL))
    }
  }
  if (show_progress) {
    cli::cli_alert_info("Uploading {.file {name}} ...")
  }
  releases <- pb_releases2(repo = repo,
                           .api_url = .api_url,
                           .token = .token)
  upload_url <- releases$upload_url[releases$tag_name == tag]
  r <- httr::RETRY(
    verb = "POST", url = sub("\\{.+$", "", upload_url),
    query = list(name = name),
    httr::add_headers(Authorization = paste("token",.token)),
    body = httr::upload_file(file_path),
    progress,
    terminate_on = c(400, 401, 403, 404, 422)
  )
  if (show_progress) {
    httr::warn_for_status(r)
  }
  # .pb_cache_clear()
  invisible(r)
}

# used by pb_info2
get_release_assets2 <- function(releases, r,
                                .api_url = gh:::default_api_url(),
                                .token = gh::gh_token(.api_url)) {
  if (nrow(releases) == 0) {
    return(data.frame())
  }
  asset_list <- vector("list", length = nrow(releases))
  for (i in seq_along(releases$tag_name)) {
    a <- gh::gh(
      endpoint = "/repos/:owner/:repo/releases/:release_id/assets",
      owner = r[[1]], repo = r[[2]],
      release_id = releases$release_id[[i]],
      .limit = Inf,
      .api_url = .api_url,
      .token = .token,
      .progress = TRUE
    )
    if (length(a) == 0) {
      next
    }
    if (!identical(a[[1]], "")) {
      a_df <- data.frame(
        file_name = vapply(a, `[[`, character(1),"name"),
        size = vapply(a, `[[`, integer(1), "size"),
        timestamp = lubridate::as_datetime(vapply(a,`[[`, character(1), "updated_at")),
        tag = releases$tag_name[i],
        owner = r[[1]],
        repo = r[[2]],
        upload_url = releases$upload_url[i],
        browser_download_url = vapply(a, `[[`, character(1L),"browser_download_url"),
        id = vapply(a, `[[`,integer(1L), "id"),
        state = vapply(a, `[[`,character(1L), "state"), stringsAsFactors = FALSE)
      asset_list[[i]] <- a_df
    }
  }
  release_assets <- do.call(rbind, asset_list)
  return(release_assets)
}

# uses pb_releases2, pb_release_create2
pb_upload2 <- function (file,
                        repo = paste(gh::gh_tree_remote(), collapse = "/"),
                        tag = "latest", name = NULL,
                        overwrite = "use_timestamps",
                        use_timestamps = NULL,
                        show_progress = TRUE,
                        .api_url = gh:::default_api_url(),
                        .token = gh::gh_token(.api_url),
                        dir = NULL) {
  stopifnot(is.character(repo), is.character(tag), length(tag) == 1, length(repo) == 1)
  releases <- pb_releases2(repo = repo,
                           .api_url = .api_url,
                           .token = .token)
  if (tag == "latest" && length(releases$tag_name) > 0 &&
      !"latest" %in% releases$tag_name) {
    if (getOption("piggyback.verbose", default = interactive())) {
      cli::cli_alert_info("Uploading to latest release: {.val {releases$tag_name[[1]]}}.")
    }
    tag <- releases$tag_name[[1]]
  }
  if (!tag %in% releases$tag_name && !interactive()) {
    cli::cli_abort("Release {.val {tag}} not found in {.val {repo}}. No upload performed.")
  }
  if (!tag %in% releases$tag_name) {
    cli::cli_alert_warning("Release {.val {tag}} not found in {.val {repo}}, creating.")
    pb_release_create2(repo = repo, tag = tag,
                       .api_url = .api_url,
                       .token = .token)
    # cli::cli_alert_warning("Release {.val {tag}} not found in {.val {repo}}.")
    # run <- utils::menu(choices = c("Yes", "No"), title = glue::glue("Would you like to create a new release now?"))
    # if (run == 2)
    #   return(invisible(NULL))
    # if (run == 1)
    #   pb_release_create2(repo = repo, tag = tag,
    #                      .api_url = .api_url,
    #                      .token = .token)
    Sys.sleep(2)
  }
  out <- lapply(file, function(f) pb_upload_file2(file=f, repo=repo, tag=tag, name=name,
                                                  overwrite=overwrite,
                                                  use_timestamps=use_timestamps, show_progress=show_progress,
                                                  .token=.token,
                                                  .api_url=.api_url,
                                                  dir=dir))
  invisible(out)
}

# used by pb_upload2
pb_release_create2 <- function (repo = paste(gh::gh_tree_remote(), collapse = "/"),
                                tag, commit = NULL,
                                name = tag,
                                body = "Data release",
                                draft = FALSE,
                                prerelease = FALSE,
                                .api_url = gh:::default_api_url(),
                                .token = gh::gh_token(.api_url)) {
  releases <- pb_releases2(repo = repo,
                           .token = .token,
                           .api_url=.api_url,
                           verbose = FALSE)
  if (nrow(releases) > 0 && tag %in% releases$tag_name) {
    cli::cli_warn("Failed to create release: {.val {tag}} already exists!")
    return(invisible(releases[tag %in% releases$tag_name, ]))
  }
  r <- parse_repo2(repo)
  payload <- compact(list(tag_name = tag, target_commitish = commit,
                          name = name, body = body, draft = draft, prerelease = prerelease))
  # .api_url <- gh:::default_api_url()
  resp <- httr::RETRY(verb = "POST", url = glue::glue("{.api_url}/repos/{r[[1]]}/{r[[2]]}/releases"),
                      httr::add_headers(Authorization = paste("token", .token)),
                      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
                      terminate_on = c(400, 401, 403, 404, 422))
  if (httr::http_error(resp)) {
    cli::cli_warn(c(`!` = "Failed to create release: HTTP error {.val {httr::status_code(resp)}}.",
                    "See returned error messages for more details"))
    return(httr::content(resp))
  }
  # .pb_cache_clear()
  release <- httr::content(resp)
  cli::cli_alert_success("Created new release {.val {name}}.")
  return(invisible(release))
}

# 1. uses gh_download_asset2, pb_info2
pb_download2 <- function(file = NULL, dest = ".",
                         repo = paste(gh::gh_tree_remote(), collapse = "/"),
                         tag = "latest",
                         overwrite = TRUE, ignore = "manifest.json", use_timestamps = TRUE,
                         show_progress = TRUE,
                         .api_url = gh:::default_api_url(),
                         .token = gh::gh_token(.api_url)) {
  progress <- httr::progress("down")
  if (!show_progress) {
    progress <- NULL
  }
  df <- pb_info2(repo=repo, tag=tag,
                 .api_url=.api_url,
                 .token=.token)
  df <- df[df$state != "starter", ]
  if (!is.null(file)) {
    i <- which(df$file_name %in% file)
    if (length(i) < 1) {
      cli::cli_warn("file(s) {.file {file}} not found in repo {.val {repo}}")
    }
    df <- df[i, ]
  } else {
    i <- which(df$file_name %in% ignore)
    if (length(i) >= 1) {
      df <- df[-i, ]
    }
    file <- df$file_name
  }
  if (length(dest) == 1) {
    i <- which(df$file_name %in% file)
    dest <- file.path(dest, df$file_name[i])
  }
  df$dest <- dest
  if (use_timestamps) {
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE
    df <- df[update, ]
    if (dim(df)[[1]] < 1) {
      cli::cli_alert_info("All local files already up-to-date!")
      return(invisible(NULL))
    }
  }
  resp <- lapply(seq_along(df$id), function(i) {
    gh_download_asset2(owner=df$owner[[1]],
                       repo=df$repo[[1]],
                       id = df$id[i], destfile = df$dest[i], overwrite = overwrite,
                       .api_url=.api_url,
                       .token = .token, progress = progress
    )
  })
  return(invisible(resp))
}

# used by pb_download2
gh_download_asset2 <- function(owner, repo, id, destfile,
                               overwrite = TRUE,
                               .api_url = gh:::default_api_url(),
                               .token = gh::gh_token(.api_url),
                               progress = httr::progress("down")) {
  if (fs::file_exists(destfile) && !overwrite) {
    cli::cli_warn(c(
      `!` = "{.val {destfile}} already exists, skipping download.",
      "Set {.code overwrite = TRUE} to overwrite files."
    ))
    return(NULL)
  }
  if (!is.null(progress)) {
    cli::cli_alert_info("Downloading {.val {basename(destfile)}}...")
  }
  auth_token <- if (!is.null(.token) && .token != "") {
    httr::add_headers(Authorization = paste("token", .token))
  }
  # .api_url <- gh:::default_api_url()
  resp <- httr::RETRY(
    verb = "GET",
    url = paste0(.api_url,"/repos/", owner, "/", repo, "/", "releases/assets/",id),
    httr::add_headers(Accept = "application/octet-stream"),
    auth_token, httr::write_disk(destfile, overwrite = overwrite),
    progress
  )
  if (resp$status_code == 400) {
    resp <- httr::RETRY(verb = "GET",
                        url = resp$url, httr::add_headers(Accept = "application/octet-stream"),
                        auth_token, httr::write_disk(destfile, overwrite = TRUE),
                        progress)
  }
  # if (TRUEgetOption("piggyback.verbose", default = TRUE)) {httr::warn_for_status(resp)}
  httr::warn_for_status(resp)
  invisible(resp)
}



get_gitrepo <- function(path=".") {
  tryCatch({
    paste(gh::gh_tree_remote(path), collapse = "/")
  },error=function(e) {
    stop("YTError: No Git repo detected.")
  })
}

get_giturl <- function(path=".") {
  tryCatch({
    gert::git_remote_list(repo=path)$url
  },error=function(e) {
    stop("YTError: no git URL detected")
  })
}

get_gitapi <- function(path=".") {
  git.url <- get_giturl(path=path)
  git.base <- str_extract(git.url,"https?://[^/]+")
  if (git.base=="https://github.com") {
    git.api <- "https://api.github.com"
  } else {
    git.api <- paste0(git.base,"/api/v3")
  }
  return(git.api)
}

get_gittoken <- function(path=".") {
  tryCatch({
    api <- get_gitapi(path=path)
    gh::gh_token(api_url = api)
  },error=function(e) {
    NULL
  })

}


#' @export
#' @param files File(s) or folder(s) to be uploaded (absolute path or relative to `path`).
#' Default is to upload all files found in `<path>/release_data`.
#' @rdname git_release
upload_git_release <- function(files="release_data",
                               path = here::here(),
                               tag = "v0.0.0.1",
                               generate_load_script = "R/run_this_to_download_data.R",
                               repo = get_gitrepo(path=path),
                               api = get_gitapi(path=path),
                               token = get_gittoken(path=path)) {
  # note to self: should probably check if in repo, and token works.
  # linux doesn't seem to work if it is MSK git.. need token or something
  # path="C:/Users/Ying/R/autofmt"
  # files=c("data/fmt.study.compact.RData","data/fmt.tax.blast.full.rds")
  # files="data/fmt.study.compact.RData"
  # files=c("data","R")
  # files="data"
  # files="autofmt.Rproj"
  # files="/"
  # convert folders to files, make absolute paths
  filelist <- map(files,~{
    if (R.utils::isAbsolutePath(.x)) {
      path <- .x
    } else {
      path <- file.path(path,.x)
    }
    if (dir.exists(path)) {
      # remove subfolders
      dirfiles <- setdiff(list.files(files,full.names=TRUE),list.dirs(files,full.names = TRUE))
      return(dirfiles)
    } else if (file.exists(path)) {
      return(path)
    } else {
      cli::cli_abort("YTError: path does not exist: {.file {path}}")
    }
  }) %>% list_c()

  if (length(filelist)==0) {
    cli::cli_abort("No files found in {dir}")
  }
  base_files <- basename(filelist)
  if (anyDuplicated(base_files)) {
    filelist <- filelist[order(base_files)]
    dups <- filelist[duplicated(base_files) | duplicated(base_files,fromLast = TRUE)]
    cli::cli_abort("YTError: these files have duplicated basenames: {.file {dups}}")
  }
  cli::cli_alert_info("Uploading files as Git release: {.file {base_files}}")

  pb_upload2(filelist,
             tag=tag,
             repo=repo,
             .api_url = api,
             .token=token)

  if (!is.null(generate_load_script)) {
    if (!file.exists(generate_load_script)) {
      script_dir <- dirname(generate_load_script)
      dir.create(script_dir, showWarnings = FALSE)
      code <- str_glue('
# Github release data is stored.
# To download the data from Github and store in data folder, run code below:

# Git API: {api}
# Repo: {repo}
# Data files: {paste(base_files,collapse=', ')}

# Note that that yingtools2 0.0.1.174 or higher is needed.
# This will place files in folder release_data.
if (FALSE) {
  if (!require("yingtools2") || packageVersion("yingtools2")<"0.0.1.174") {{
   remotes::install_github("ying14/yingtools2")
  }}
  yingtools2::download_git_release()
}
')
      writeLines(code, generate_load_script)
      cli::cli_alert_info("Data loading script generated: {.path {generate_load_script}}")
      cli::cli_alert_info("Consider adding these to {.path .gitignore}:\n{.path {files}}")
    }
  }
}



#' Download/Upload Git Release Ddata
#'
#' @description
#' Use these functions to store/access data on Git repositories, through release asset.
#' This essentially what [`piggyback`](https://github.com/ropensci/piggyback) does, but with modifications to work with Github Enterprise servers.
#'
#' @details
#' Storing data on Github repostories is quite limited, to 50 Mb per file. Also, each time you push a new version of the data,
#' there are multiple copies which can accumulate over time. However, you can upload the data as a *release asset*, which does not
#' have size limits, and which can be overwritten each time, so as to avoid duplicate copies.
#'
#' The [`piggyback`](https://github.com/ropensci/piggyback) package has great functions to take advantage of the *release asset* feature.
#' However, it currently only works with Github, and does not play well with Github Enterprise.
#' These functions represent modifications to make it work, and to streamline the upload/download process.
#'
#' Some of the default settings are useful for avoiding potential issues.
#' The default `tag` of `"v0.0.0.1"` is good in case the Git repo/project is an
#' R package with formal releases, where automated systems are looking for latest version
#' and you would not want it to pick up the release data by accident.
#' Data is stored in the default `dest` location `release_data`.
#'
#' You probably want to avoid storing in `data`, because if the repo/project is an
#' R package, it will try to include the release data in the package during
#' build (changing .Rbuildignore doesn't seem to work).
#'
#' @param tag The tag version to upload/download. Default is to use `"v0.0.0.1"`.
#' When downloading, can specify `"latest"` (i.e. latest version).
#' @param dest The folder where release data will be saved. Default is `release_data`.
#' @param path Path of the git repo. Default is current project's directory, `here::here()`.
#' @param repo string: GH repository name in format `"owner/repo"`. Default is to guess based on `path`.
#' @param api GitHub API URL. For standard Github this would be `"https://api.github.com"`,
#' but in Github Enterprise this would be something like `"https://github.XXXXX.org/api/v3"`.
#' Default is to guess based on info in the Git repo `path`.
#' @param token Personal access token (PAT). These credentials may be needed, depending on the operation and repo.
#' Default is to use [gh::gh_token()], if it exists, or `NULL` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' mt1 <- mtcars %>% mutate(version=1)
#' iris1 <- iris %>% mutate(version=1)
#' # save files to the /data folder
#' write_csv(mt1,file="data/mt1.csv")
#' write_csv(iris1,file="data/iris1.csv")
#' # upload files in /data to Github as a release
#' upload_git_release()
#'
#' # In a system that doesn't have the data, download files from Github
#' download_git_release()
#' }
#' @rdname git_release
download_git_release <- function(tag = "v0.0.0.1",
                                 dest = "release_data",
                                 path = here::here(),
                                 repo = get_gitrepo(path=path),
                                 api = get_gitapi(path=path),
                                 token = get_gittoken(path=path)) {
  if (!dir.exists(dest)) {
    cli::cli_alert_info("Creating folder {.path {dest}}.")
    dir.create(dest, showWarnings = FALSE)
  }
  pb_download2(tag=tag,
               dest=dest,
               repo=repo,
               .api_url = api,
               .token=token)
}


# if (FALSE) {
#   mt1 <- mtcars %>% mutate(version=1)
#   iris1 <- iris %>% mutate(version=1)
#   write_csv(mt1,file="mt1.csv")
#   write_csv(iris1,file="iris1.csv")
#   mt2 <- mtcars %>% mutate(version=2)
#   iris2 <- iris %>% mutate(version=2)
#   write_csv(mt2,file="mt2.csv")
#   write_csv(iris2,file="iris2.csv")
#
#   upload_git_release(c("mt1.csv","iris1.csv"))
#
#   saved_files
# debugonce(upload_git_release)
#
#   download_git_release()
  # ytnotes
#
#   upload_git_release(c("mt1.csv","iris1.csv"))
#   download_git_release()
#
#
# }

#' Get git info on all git folders
#'
#' `r lifecycle::badge('experimental')`
#' @param parent.dir path containing all github folders
#'
#' @return Table containing data on all git folders
#' @export
#'
#' @examples
#' get_getdir_info("C:/Users/Ying/R")
get_gitdir_info <- function(parent.dir = ".") {
  # parent.dir <- "C:/Users/Ying/R"
  dirs <- list.dirs(parent.dir,recursive=FALSE)
  get.status <- function(path) {
    # path = tbl$path[1]
    if (length(path)>1) {
      return(map_chr(path,get.status))
    }
    gitdir <- file.path(path,".git")
    projfile <- file.path(path,paste0(basename(path),".Rproj"))
    if (!(dir.exists(path) && dir.exists(gitdir) && file.exists(projfile))) {
      return(NA_character_)
    }
    old.dir <- getwd()
    setwd(path)
    status <- gitr::gst() %>% paste(collapse="\n")
    setwd(old.dir)
    return(status)
  }
  tbl <- tibble(path=dirs) %>%
    mutate(base=basename(dirs),
           status.text=get.status(path),
           branch.uptodate=status.text %like% "branch is up to date",
           nothing.to.commit=status.text %like% "nothing to commit",
           git.uptodate=branch.uptodate & nothing.to.commit,
           NULL) %>%
    filter(!is.na(status.text)) %>%
    mutate(url=map_chr(path,yingtools2:::get_giturl)) %>%
    select(base,path,git.uptodate,status.text,everything())
  return(tbl)
}

