#' Get data from NorSpis
#'
#' Functions to query data from a database holding NorSpis data at Rapporteket.
#' Providing these function with the shiny session object logging of queries
#' will also be performed
#'
#' @param registry_name Character string with name of the registry from which
#' data are to be queried
#' @param resh_id Integer providing the organization id. Usefull for data
#' filtering
#' @param ... Optional arguments to pass to the function. If \code{session} is
#' provided this will be assumed a valid shiny session object and hence logging
#' will be performed
#'
#' @name query_data
#' @aliases query_alle_scorer query_behandling query_behandling_num
#' query_enkelt_ledd query_enkelt_ledd_num forlops_oversikt
#' @return Data frame of registry data
NULL


#' @rdname query_data
#' @export
query_alle_scorer <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM AlleScorer;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load AlleScorer data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}


#' @rdname query_data
#' @export
query_behandling <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM Behandling;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load Behandling data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}


#' @rdname query_data
#' @export
query_behandling_num <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM BehandlingNum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load BehandlingNum data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}


#' @rdname query_data
#' @export
query_enkelt_ledd <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM EnkeltLedd;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load EnkeltLedd data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}


#' @rdname query_data
#' @export
query_enkelt_ledd_num <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM EnkeltLeddNum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load EnkeltLeddNum data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}


#' @rdname query_data
#' @export
query_forlops_oversikt <- function(registry_name, resh_id, ...) {

  query <- "SELECT * FROM ForlopsOversikt;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load ForlopsOversikt data from ", registry_name, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}
