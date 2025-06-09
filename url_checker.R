library(httr2)
library(rvest)
library(XML)

get_domain <- function(url){
    domain <- str_match(url, "(?<base>https?://(?:www.)?[a-zA-Z0-9-.]{1,256}\\.[a-zA-Z0-9]{1,6})/?(?<path>.*)")[,"base"]

    if_else(endsWith(domain, "/"), domain, paste0(domain, "/"))
}

get_path <- function(url){
    str_match(url, "(?<base>https?://(?:www.)?[a-zA-Z0-9-.]{1,256}\\.[a-zA-Z0-9]{1,6})/?(?<path>.*)")[,"path"]
}

get_path_ending <- function(url){
    str_match(url, "(.*)/(?<final>[a-zA-Z0-9]{1,256}\\.[a-zA-Z0-9]{1,6})?")[,"final"]
}

strip_path_ending <- function(url){
    pe <- get_path_ending(url)

    ans <- if_else(is.na(pe), url, str_replace(url, fixed(pe), ""))

    if_else(endsWith(ans, "/"), ans, paste0(ans, "/"))
}

drop_trailing_dir <- function(url){
    domain <- get_domain(url)

    url <- strip_path_ending(url)

    up <- str_match(url, "(?<up>.*/)(?<trailing>.+)")[,"up"]

    if_else(nchar(domain) > nchar(up), domain, up)
}

join_url_parts_ <- function(x, base){
    if(missing(base)) stop(sQuote("base"), "must be provided.")

    url <- base

    if(startsWith(x, "http")){
        return(x)
    }

    dots <- str_split(x, "/")[[1]]

    for(d in dots){
        if(d == "."){
            url <- strip_path_ending(url)
        } else if(d == ".."){
            url <- drop_trailing_dir(url)
        } else {
            url <- paste0(strip_path_ending(url), d)
        }
    }

    url
}

join_url_parts <- Vectorize(join_url_parts_, "x")

stopifnotequal <- function(correct, ...){
    dots <- list(...)
    for(d in dots){
        if(correct != d){
            stop(sQuote(d), " is not equal to expected value ", sQuote(correct), call. = FALSE)
        }
    }
}

stopifnotequal(
    "https://michael-weylandt.com/syllabus.html",
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com/index.html"),
    join_url_parts("././syllabus.html", base="https://michael-weylandt.com/index.html"),
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com"),
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com/")
)

stopifnotequal(
    "https://michael-weylandt.com/STA9750/syllabus.html",
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com/STA9750/index.html"),
    join_url_parts("././syllabus.html", base="https://michael-weylandt.com/STA9750/index.html"),
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com/STA9750"),
    join_url_parts("./syllabus.html", base="https://michael-weylandt.com/STA9750/")
)


stopifnotequal(
    "https://michael-weylandt.com/teaching.html",
    join_url_parts("../teaching.html", base="https://michael-weylandt.com/STA9750/index.html"),
    join_url_parts(".././teaching.html", base="https://michael-weylandt.com/STA9750/index.html"),
    join_url_parts("./../teaching.html", base="https://michael-weylandt.com/STA9750/index.html"),
    join_url_parts("../teaching.html", base="https://michael-weylandt.com/STA9750"),
    join_url_parts("../teaching.html", base="https://michael-weylandt.com/STA9750/")
)



pull_href_ <- function(url){
    #if(!str_ends(url, "/")) url <- paste0(url, "/")

    resp <- request(url) |>
        req_perform()

    if(str_detect(resp |> resp_header("Content-Type"), "text/html", negate=TRUE)){
        data.frame(text=character(),
                   href=character(),
                   status=logical())
    } else {
        links <- resp |>
            resp_body_html() |>
            html_elements("a")

        tibble(text = links |> html_text2(),
               found_at = url,
               href = links |>
                   html_attr("href") |>
                   join_url_parts(base=url) |>
                   str_replace("\\#.*", "") |>
                   unname()
               ) |>
            distinct(href, .keep_all=TRUE) |>
            remove_rownames()
    }
}
pull_href <- function(url) map(url, pull_href_, .progress=TRUE) |> list_rbind()

check_url_ <- function(x){
    !(request(x) |> req_error(is_error = \(x) FALSE) |> req_perform() |> resp_is_error())
}

check_url <- function(x) map_lgl(x, possibly(check_url_, otherwise=FALSE), .progress=TRUE)

BASE_URL <- "https://michael-weylandt.com/"
REGISTRY <- data.frame(text=character(),
                       href=character(),
                       status=logical())

URLS_CHECKED <- character()
N_ROUNDS <- 0

repeat{
    N_ROUNDS <- N_ROUNDS + 1
    cat("Starting URL Check Round", N_ROUNDS, "\n")
    if(NROW(REGISTRY) == 0){
        REGISTRY <- pull_href(BASE_URL) |>
            mutate(status = check_url(href)) |>
            filter(!str_detect(href, "mailto")) |>
            filter(!str_detect(href, "tel:"))
        URLS_CHECKED <- BASE_URL
    } else {
        URLS_TO_CHECK <- setdiff(REGISTRY |>
                                     filter(status == TRUE) |>
                                     filter(str_detect(href, BASE_URL)) |>
                                     pull(href),
                                 URLS_CHECKED) |>
            unique()

        cat(" - Checking", length(URLS_TO_CHECK), "new URLs.\n")
        cat(paste0("   - ", URLS_TO_CHECK, "\n"), sep="")

        if(length(URLS_TO_CHECK) == 0){
            break
        }

        NEW_REGISTRY <- pull_href(URLS_TO_CHECK) |>
            mutate(status = check_url(href)) |>
            filter(!str_detect(href, "mailto")) |>
            filter(!str_detect(href, "tel:"))

        REGISTRY <- rbind(REGISTRY, NEW_REGISTRY) |>
            distinct(href, .keep_all=TRUE)
        URLS_CHECKED <- c(URLS_CHECKED, URLS_TO_CHECK)
    }

    if(N_ROUNDS > 2)
        break
}

REGISTRY |> filter(!status) |> pull(href)
REGISTRY |> filter(!status) |> View()
