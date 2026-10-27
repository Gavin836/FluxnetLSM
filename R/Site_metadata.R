# Author: ned haughton

# Uses rvest library for read_html, html_node, html_attr, html_table


#################################################
# General metadata functions
################################################

#' Empty site metadata template, including only the site code
#'
#' @return metadata list
site_metadata_template <- function(site_code) {
    metadata <- list(
        SiteCode = site_code,
        Fullname = NA,
        SiteLatitude = NA,
        SiteLongitude = NA,
        SiteElevation = NA,
        IGBP_vegetation_short = NA,
        IGBP_vegetation_long = NA,
        TowerHeight = NA,
        CanopyHeight = NA,
        Tier = NA,
        Exclude = FALSE,
        Exclude_reason = NA,
        Description = NA,
        TowerStatus = NA,
        Country = NA,
        MeasurementHeight = NA,
        VegetationDescription = NA,
        SoilType = NA,
        Disturbance = NA,
        CropDescription = NA,
        Irrigation = NA,
        ## New bif variables
        Vegetation_height = NA,
        Source = NA,
        Reference = NA,
        ACKNOWLEDGEMENT = NA,
        MAT = NA,
        MAP = NA,
        CLIMATE_KOEPPEN = NA,
        DOI = NA,
        DOI_CITATION = NA,
        DOI_CONTRIBUTOR_NAME = NA,
        DOI_CONTRIBUTOR_EMAIL = NA,
        DOI_CONTRIBUTOR_INSTITUTION = NA,
        UTC_OFFSET = NA
    )

    return(metadata)
}


#' Get site name from metadata
get_site_code <- function(metadata){
    return(metadata[["SiteCode"]])
}


#' Gets the git version from the installed package
#' See src/zzz.R for how git revision is discovered
get_git_version <- function() {

    #Initialise warnings
    warnings <- ""

    desc <- read.dcf(system.file("DESCRIPTION", package = "FluxnetLSM"))
    if ("git_revision" %in% colnames(desc)) {
        git_rev <- desc[1, "git_revision"]
    } else if ("RemoteSha" %in% colnames(desc)) {
        git_rev <- desc[1, "RemoteSha"]
    } else {
        git_rev <- "UNKNOWN"

        warn <- paste("Unknown git revision of FluxnetLSM. Please",
                      "visit https://github.com/aukkola/FluxnetLSM and",
                      "review the installation procedure")
        warnings <- append_and_warn(warn=warn, warnings)
    }
    return(list(git_rev=git_rev, warn=warnings))
}


#' Adds processor metadata, including processor version
#'
#' @return metadata list
add_processing_metadata <- function(metadata) {

    #return git revision and warning if revision found
    git_rev = get_git_version()

    metadata$Processing <- list(
        processor = "FluxnetLSM",
        URL = "https://github.com/aukkola/FluxnetLSM",
        version = packageVersion("FluxnetLSM"),
        git_rev = git_rev$git_rev
    )

    return(list(out=metadata,warn=git_rev$warn))
}


#' Updates old metadata with new metadata, ignoring NAs, and warning on differences
#'
#' @return metadata list
#' @export
update_metadata <- function(metadata, new_metadata, overwrite=TRUE) {
    for (n in names(new_metadata)) {
        if (!is.na(new_metadata[[n]])) {  # Don't overwrite with empty data
            if (n %in% names(metadata) && !is.na(metadata[[n]])) { # Old data already exists, check
                different <- FALSE
                if (!is.numeric(metadata[[n]])) {
                    if (new_metadata[[n]] != metadata[[n]]) { # non-numeric and different
                        different <- TRUE
                    }
                } else { # numeric
                    if (round(new_metadata[[n]], 4) != round(metadata[[n]], 4)) { # and different
                        different <- TRUE
                    }
                }

                if (different) {
                    overwrite_text = if (overwrite) "Overwriting" else "Not overwriting"

                    warn <- paste0("New metadata for ", n, " has different values! ",
                                   overwrite_text, ".\n",
                                   "  old: ", metadata[n],
                                   ", new: ", new_metadata[n])
                    message(warn)

                    if (overwrite) {
                        metadata[n] <- new_metadata[n]
                    }

                }
            } else {  # old data is empty, or doesn't exist
                metadata[n] <- new_metadata[n]
            }

        ## Metadata value is missing but the field doesn't exist in metadata,
        ## save as is to avoid losing metadata field
        } else if (is.na(new_metadata[[n]]) & !(n %in% names(metadata))){
            metadata[n] <- new_metadata[n]
        }
    }

    return(metadata)
}

################################################
# CSV-stored metadata
################################################

# Find site info file path (not using data() command directly because reads a CSV with a
# semicolon separator and this leads to incorrect table headers)

#' Tries to gather metadata from the included site CSV
#'
#' @return metadata list
#' @export

get_site_metadata_from_CSV <- function(
    metadata = NA,
    incl_processing = TRUE,
    model,
    site_csv_file = system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM")
    ) {

    if (!is.list(metadata)) {
        metadata <- site_metadata_template(metadata)
    }

    # allow for an external file to be called instead of a
    # a hard coded one using a new argument
    csv_data <- try(read.csv(site_csv_file, header = TRUE,
                    stringsAsFactors = FALSE))

    if (is.na(metadata[1])) {  # [1] to skip if site_code is set
        # get all existing metadata as a list of lists
        message("Loading metadata for all sites from csv_data cache (", site_csv_file, ")")
        metadata <- lapply(row.names(csv_data), function(row) {
            as.list(csv_data[row, ])
        })
        names(metadata) <- csv_data$SiteCode
        return(metadata)
    }

    site_code <- get_site_code(metadata)

    message("Loading metadata for ", site_code, " from csv_data cache (", site_csv_file, ")")

    #Found site code in CSV
    if (site_code %in% csv_data$SiteCode) {
        csv_row <- as.list(csv_data[csv_data$SiteCode == site_code, ])
        metadata = update_metadata(metadata, csv_row)

    #Didn't find it (stop if trying to pass model parameters)
    } else {
        message("    ", site_code, " not found in csv_data file")

        if(!is.na(model)) stop("Cannot read model parameters, site not found in CSV metadata file. ",
                             "Please amend CSV file or set model=NA")
    }

    if (incl_processing) {
        warnings <- ""
        warn_missing_metadata(metadata)

        metadata <- add_processing_metadata(metadata)
        warnings <- append_and_warn(warn=metadata$warn, warnings, call=FALSE)
        metadata <- metadata$out

        return(list(out=metadata,warn=warnings))
    } else {
        return(metadata)
    }
}


#' Writes metadata to CSV, only updating non-NA data
save_metadata_to_csv <- function(metadata) {
    save_metadata_list_to_csv(list(metadata))
}


#' Write multiple site metadata to list at once
save_metadata_list_to_csv <- function(metadata_lists) {
    old_csv_data <- read.csv(site_csv_file, header = TRUE,
                         stringsAsFactors = FALSE)

    new_csv_data <- metadata_list_to_dataframe(metadata_lists)

    common_names <- intersect(names(old_csv_data), names(new_csv_data))
    common_names <- common_names[common_names != "SiteCode"]

    # Merge new and existing datasets, preferring new.
    csv_data <- merge(new_csv_data, old_csv_data,
                      all = TRUE, by = "SiteCode")
    for (n in common_names) {
        # Use old data only if new data is missing.
        csv_data[[n]] <- ifelse(is.na(csv_data[[paste0(n, ".x")]]),
                                csv_data[[paste0(n, ".y")]],
                                csv_data[[paste0(n, ".x")]])
        csv_data[[paste0(n, ".x")]] <- NULL
        csv_data[[paste0(n, ".y")]] <- NULL
    }

    csv_data <- csv_data[order(csv_data$SiteCode), ]

    # Fix new lines in Descriptions
    csv_data$Description <- gsub("\\n", "\\\\n", csv_data$Description)

    write.csv(csv_data, file = site_csv_file, row.names = FALSE)
}


#' Convert a list of metadata lists to a dataframe
metadata_list_to_dataframe <- function(metadata_lists) {

    to_save <- list("SiteCode", "Fullname", "Description", "TowerStatus",
                    "Country", "SiteLatitude", "SiteLongitude", "SiteElevation",
                    "IGBP_vegetation_short", "IGBP_vegetation_long",
                    "TowerHeight", "CanopyHeight", "Tier"
                    )

    csv_data <- data.frame()

    for (metadata in metadata_lists) {
        site_code <- metadata$SiteCode
        for (v in to_save) {
            if (v %in% names(metadata) && !is.na(metadata[[v]])) {
                csv_data[site_code, v] <- metadata[[v]]
            }
        }
    }

    return(csv_data)
}


#' Reads all ORNL data into the CSV file
#' @export
update_csv_from_web <- function() {
    csv_data <- get_site_metadata_from_CSV(incl_processing=FALSE)

    csv_site_codes <- names(csv_data)

    fluxdata_site_codes <- get_fluxdata_org_site_codes()
    ornl_site_codes <- get_ornl_site_codes()

    all_site_codes <- union(union(csv_site_codes, fluxdata_site_codes),
                            ornl_site_codes)

    metadata <- list()
    for (sc in all_site_codes) {
        site_md <- site_metadata_template(sc)
        if (sc %in% csv_site_codes) {
            site_md <- update_metadata(site_md, csv_data[[sc]])
        }
        if (any(check_missing(site_md)) && sc %in% fluxdata_site_codes) {
            # Overwrite with fluxdata.org data
            site_md <- get_fluxdata_org_site_metadata(site_md)
        }
        #if (any(check_missing(site_md)) && sc %in% ornl_site_codes) {
        #    # Don't overwrite with ORNL data, just gapfill
        #    site_md <- get_ornl_site_metadata(site_md, overwrite = FALSE)
        #}
        metadata[[sc]] <- site_md
    }

    message("Saving metadata to ", site_csv_file)
    save_metadata_list_to_csv(metadata)
}

################################################
# Web-based metadata
################################################

### fluxnet.ornl.gov ###

#' Get all available site codes from site_status table
get_ornl_site_codes <- function() {

    library(rvest)
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    table_data <- page_html %>% html_node("#historical_site_list") %>% html_table()

    site_codes <- sort(table_data[["FLUXNET ID"]])

    return(site_codes)

}


#' Get a single ORNL site URL from site_status table
get_site_ornl_url <- function(site_code) {
    ornl_url <- get_ornl_site_url_list(list(site_code))[[site_code]]
    return(ornl_url)
}


#' Get a list of ORNL site URLs from site_status table
get_ornl_site_url_list <- function(site_code_list) {

    library(rvest)
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    ornl_url_list <- list()

    for (site_code in site_code_list) {
        # looks for table cell with site code as contents, then looks up the parent
        # row, and finds the href of the first link.
        xpath <- paste0("//td[text()='", site_code, "']/..")
        trow <- tryCatch(page_html %>% html_node(xpath = xpath), error=function(e) NULL)
        if (class(trow) == "xml_node") {
            ornl_rel_url <- trow %>% html_node("a") %>% html_attr("href")
            ornl_url_list[[site_code]] <- paste0("https://fluxnet.ornl.gov/", ornl_rel_url)
        } else {
            message(site_code, " not found in table at https://fluxnet.ornl.gov/site_status")
        }
    }

    return(ornl_url_list)
}


#' Get metadata from ORNL
#'
#' @return metadata list
get_ornl_site_metadata <- function(metadata, site_url=NULL, overwrite=TRUE) {

    library(rvest)
    site_code <- get_site_code(metadata)

    if (is.null(site_url)) {
        site_url <- get_site_ornl_url(site_code)
        if (is.null(site_url)) {
            # site not found at ORNL
            return(metadata)
        }
        metadata$ORNL_URL <- site_url
    }

    message("Trying to load metadata for ", site_code, " from ORNL (", site_url, ")")

    page_html <- read_html(site_url)

    new_metadata = list()
    # General info
    table_data <- page_html %>% html_node("table#fluxnet_site_information") %>% html_table()
    new_metadata$Fullname <- table_data[table_data[1] == "Site Name:"][2]
    new_metadata$Description <- table_data[table_data[1] == "Description:"][2]
    new_metadata$TowerStatus <- table_data[table_data[1] == "Tower Status:"][2]

    # Location Information
    table_data <- page_html %>% html_node("table#fluxnet_site_location_information") %>% html_table()
    new_metadata$Country <- table_data[table_data[1] == "Country:"][2]
    lat_lon <- strsplit(table_data[table_data[1] == "Coordinates:(Lat, Long)"][2], ", ")[[1]]
    new_metadata$SiteLatitude <- round(as.numeric(lat_lon[1]), 5)  # round to ~1m
    new_metadata$SiteLongitude <- round(as.numeric(lat_lon[2]), 5)

    # Site Characteristics
    tryCatch({
        table_data <- page_html %>% html_node("table#fluxnet_site_characteristics") %>% html_table()
        elevation_text <- table_data[table_data[1] == "GTOPO30 Elevation:"][2]
        new_metadata$SiteElevation <- as.numeric(gsub("m", "", elevation_text))
        new_metadata$IGBP_vegetation_long <- table_data[table_data[1] == "IGBP Land Cover:"][2]
    }, error = function(cond) {
        message(site_code, " doesn't have a Site Characteristics table at ", site_url)
    })

    # ORNL doesn't have any of these:
    #    metadata$IGBP_vegetation_short = NULL,
    #    metadata$TowerHeight = NaN,
    #    metadata$CanopyHeight = NaN,
    #    metadata$Tier = NaN,
    #    metadata$Exclude = FALSE,
    #    metadata$Exclude_reason = NULL

    # TODO: ORNL has other potentially useful site information, affiliation info,
    # and investigator info. Should we use some?

    metadata = update_metadata(metadata, new_metadata, overwrite=overwrite)

    return(metadata)
}


### Fluxdata.org ###

#' Get all available site codes from site_status table
get_fluxdata_org_site_codes <- function() {

    library(jsonlite)

    status_JSON_url <-"https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/SiteMapData/Fluxnet"

    site_data <- jsonlite::read_json(status_JSON_url)

    site_codes <- unlist(lapply(site_data, '[[', "SITE_ID"))

    return(site_codes)

}


#' Get a single fluxdata_org site URL from site_status table
get_site_fluxdata_org_url <- function(site_code) {
    fluxdata_org_url <- paste0("http://sites.fluxdata.org/", site_code, "/")
    return(fluxdata_org_url)
}


#' Get metadata from Fluxdata.org
#'
#' @return metadata list
get_fluxdata_org_site_metadata <- function(metadata, site_url=NULL) {
    library(rvest)

    site_code <- get_site_code(metadata)

    new_metadata = list()

    if (is.null(site_url)) {
        site_url <- get_site_fluxdata_org_url(site_code)
        new_metadata$fluxdata_org_URL <- site_url
    }

    message("Trying to load new_metadata for ", site_code, " from Fluxdata.org (", site_url, ")")

    #Try to open site URL
    page_html <- tryCatch(read_html(site_url), error = function(e) NULL)

    # General info
    table_data <- tryCatch(page_html %>% html_node("table.maininfo") %>% html_table(),
                           error = function(e) NULL)
    if (class(table_data) != 'data.frame') {
        message("No data available at ", site_url, " (", class(table_data), ")")
        return(metadata)
    }

    new_metadata$Fullname <- table_data[table_data[1] == "Site Name:"][2]
    new_metadata$SiteLatitude <- as.numeric(table_data[table_data[1] == "Latitude:"][2])
    new_metadata$SiteLongitude <- as.numeric(table_data[table_data[1] == "Longitude:"][2])

    elevation_text <- table_data[table_data[1] == "Elevation (m):"][2]
    elevation <- suppressWarnings(as.numeric(gsub("m", "", elevation_text)))
    if (!is.na(elevation)) {  # lots of Fluxdata.org elevtions are missing, don't overwrite
        new_metadata$SiteElevation <- elevation
    }

    IGBP_text = strsplit(gsub("\\)", "", table_data[table_data[1] == "IGBP:"][2]), " \\(")[[1]]
    new_metadata$IGBP_vegetation_short <- IGBP_text[1]
    new_metadata$IGBP_vegetation_long <- IGBP_text[2]

    # Fluxdata.org doesn't have any of these:
    #    metadata$Description
    #    metadata$TowerStatus
    #    metadata$Country
    #    metadata$TowerHeight
    #    metadata$CanopyHeight
    #    metadata$Tier

    # TODO: fluxdata_org has other potentially useful site information, affiliation info,
    # and investigator info. Should we use some?

    metadata = update_metadata(metadata, new_metadata)

    return(metadata)
}


#' Tries to load metadata from known Fluxnet info sources on the 'web
#'
#' @return metadata list
#' @export
get_site_metadata_web <- function(metadata, incl_processing=TRUE) {

    if (!is.list(metadata)) {
        metadata <- site_metadata_template(metadata)
    }

    metadata <- get_fluxdata_org_site_metadata(metadata)

    #Commented this out as ORNL metadata no longer available
    #if (any(check_missing(metadata))) {
    #    # Don't overwrite with ORNL data, only gap-fill Fluxnet Data
    #    metadata <- get_ornl_site_metadata(metadata, overwrite=FALSE)
    #}

    # TODO: Add loaders for OzFlux, AmeriFlux, etc.

    if (incl_processing) {
        warnings <- ""
        warn_missing_metadata(metadata)

        metadata <- add_processing_metadata(metadata)
        warnings <- append_and_warn(warn=metadata$warn, warnings, call=FALSE)
        metadata <- metadata$out

        return(list(out=metadata,warn=warnings))
    } else {
        return(metadata)
    }
}


################################################
# metadata checks
################################################

#' Checks which metadata are missing (correcting for OK NAs)
#'
#' @return boolean metadata availability vector
check_missing <- function(metadata) {
    key_data = c("SiteCode", "Fullname",
                 "SiteLatitude", "SiteLongitude", "SiteElevation",
                 "IGBP_vegetation_short", "IGBP_vegetation_long",
                 "TowerHeight", "CanopyHeight", "Tier", "MeasurementHeight")

    missing_data <- is.na(metadata[key_data])

    return(missing_data)
}


#' Warns about missing metadata for the site
warn_missing_metadata <- function(metadata) {
    missing_data <- check_missing(metadata)

    if (any(missing_data)) {
        message("Missing metadata for site ", metadata$SiteCode, ":")
        message("   ", paste(names(metadata)[missing_data], collapse = ", "))
    }
}

###-----------------------------------------------------------------------------
### FLUXNET_SHUTTLE BIF PROCESSING FUNCTIONS
###-----------------------------------------------------------------------------

## Return a formated string of the heights of variable sensors. Expected up to 6 vals.
## The string values preserves order in 'vars'
instrument_height_string <- function(csv, vars) {
    target_group <- csv$GROUP_ID[csv$DATAVALUE %in% vars]
    instrument_heights <- csv$DATAVALUE[
                                 csv$GROUP_ID %in% target_group &
                                 csv$VARIABLE == 'VAR_INFO_HEIGHT'
                             ]

    format_str <- c()
    if (length(instrument_heights) != 0) {
        format_str <- paste(instrument_heights, collapse = ',')
    }

    return (format_str)
}


## Take the highest relevant sensor
calc_instrument_height <- function(bifvarinfo) {
    target_variables <- c("VPD_F", "RH")

    groups_to_keep <- unique(
        bifvarinfo$GROUP_ID[
                       bifvarinfo$VARIABLE == "VAR_INFO_VARNAME" &
                       bifvarinfo$DATAVALUE %in% target_variables
                   ]
    )

    filtered <- bifvarinfo[bifvarinfo$GROUP_ID%in% groups_to_keep, ]

    height_values <- with(
        filtered,
        DATAVALUE[VARIABLE == "VAR_INFO_HEIGHT"]
    )

    if (length(height_values) == 0L || all(is.na(height_values))) {
        NA_real_
    } else {
        as.numeric(max(height_values, na.rm = TRUE))
    }
}

## Adaptor to convert ameriflux sites into standard metadata object
## parmams: bif and bifvarinfo are dataframes
## returns: list(site, warning)
parse_ameriflux <- function(bif, bifvarinfo, site_code) {
    if (nrow(bif) == 0L || nrow(bifvarinfo) == 0L) {
        return(list(site = uest(), variables = list(), warnings =
                                                           paste("Empty bif metadata for", site_code)))
    }

    ## Check first entry since they all should be the same
    if (bif$SITE_ID[1] != site_code) {
        return(list(site = list(), variables = list(), warnings =
                                                           paste("Site code doesn't match", site_code)))
    }

    warnings <- character()
    ## lookup variable to attempt to source particular variables
    ## docs.google.com/spreadsheets/d/167CMz_39hEUZziO66yhfe5-4wabqlqUGpB19aeDp8yA
    bif_lookup <- setNames(as.character(bif$DATAVALUE), bif$VARIABLE)
    get_bif <- function(variable, default = NA_character_) {
        missing_variable <- (
            length(variable) != 1L ||
            is.na(variable) ||
            !(variable %in% names(bif_lookup))
        )

        if (missing_variable) {
            warnings <<- append(
                warnings,
                list(sprintf("Variable '%s' was not found; using default",
                            variable))
            )
            return(default)
        }

        value <- bif_lookup[[variable]]

        if (is.na(value)) {
            warnings <<- append(
                warnings,
                list(sprintf("Variable '%s' has an NA value; using default",
                            variable))
            )
            return(default)
        }

        value
    }

    ## Soil temp may exist independently of soil moisture. Assume bifvarinfo
    ## only available with the FLUXNET2015 FULLSET datasets
    dataset_vars <- get_varnames('FLUXNET2015', 'FULLSET', FALSE)
    soilt <- dataset_vars$soilt

    soil_heights <- instrument_height_string(bifvarinfo, soilt)
    if (length(soil_heights) == 0) {
        soilm <- dataset_vars$soilm
        soil_heights <- instrument_height_string(bifvarinfo, soilm)
    }

    ## Canopy/vegetation height is not always present. There may be multiple values
    ## (for different species). We will take the maximum value as the canopy height
    heightc <- unlist(
        bif_lookup[names(bif_lookup) == "HEIGHTC"],
        use.names = FALSE
    )
    max_heightc <- if (length(heightc) == 0L || all(is.na(heightc))) {
                       NA_real_
                   } else {
                       max(heightc, na.rm = TRUE)
                   }

    if (is.na(max_heightc)) {
        warnings <- append(
            warnings,
            list("Variable 'HEIGHTC' was not found; using default")
        )
    }

    height <- calc_instrument_height(bifvarinfo)

    if (is.na(height)) {
        warnings <- append(
            warnings,
            list("Variable 'VAR_INFO_HEIGHT' was not found for either 'VPD_F' or 'RH'; using default")
        )
    }

    list(
        site = list(
            Fullname = get_bif("SITE_NAME"),
            SiteLatitude = as.numeric(get_bif("LOCATION_LAT")),
            SiteLongitude = as.numeric(get_bif("LOCATION_LONG")),
            SiteElevation = as.numeric(get_bif("LOCATION_ELEV")),

            IGBP_vegetation_short = get_bif("IGBP"),
            IGBP_vegetation_long = get_bif("IGBP_COMMENT"),
            VegetationDescription = get_bif("IGBP_COMMENT"),
            CanopyHeight = max_heightc,
            Vegetation_height = max_heightc,

            Description = get_bif("SITE_DESC"),
            Disturbance = get_bif("DOM_DIST_MGMT"),
            CropDescription = get_bif("SITE_DESC"),

            Country = get_bif("COUNTRY"),
            MeasurementHeight = height,
            SoilMeasurementHeight = soil_heights,

            Source = get_bif("NETWORK"),
            Reference = get_bif("DOI_CITATION"),
            ACKNOWLEDGEMENT = get_bif("ACKNOWLEDGEMENT"),

            MAT = as.numeric(get_bif("MAT")),
            MAP = as.numeric(get_bif("MAP")),
            CLIMATE_KOEPPEN = get_bif("CLIMATE_KOEPPEN"),

            DOI = NA_character_, # can't find an example in the csv
            DOI_CONTRIBUTOR_NAME = get_bif("TEAM_MEMBER_NAME"),
            DOI_CONTRIBUTOR_EMAIL = get_bif("TEAM_MEMBER_EMAIL"),
            DOI_CONTRIBUTOR_INSTITUTION = get_bif("TEAM_MEMBER_INSTITUTION"),

            UTC_OFFSET = get_bif("UTC_OFFSET")
        ),
        warnings = warnings
    )
}

## Adaptor to convert TERN sites into standard metadata object
## parmams: bif and bifvarinfo are dataframes
## returns: list(site, warning)
parse_tern <- function(bif, bifvarinfo, site_code) {
    if (nrow(bif) == 0L || nrow(bifvarinfo) == 0L) {
        return(list(site = uest(), variables = list(), warnings =
                                                           paste("Empty bif metadata for", site_code)))
    }

    ## Check first entry since all rows have the same site ID
    if (bif$SITE_ID[1] != site_code) {
        return(list(site = list(), variables = list(), warnings =
                                                           paste("Site code doesn't match", site_code)))
    }

    issues <- list(
        invalid_key = character(),
        key_with_na = character()
    )

    ## Lookup variable to attempt to source particular variables
    ## docs.google.com/spreadsheets/d/167CMz_39hEUZziO66yhfe5-4wabqlqUGpB19aeDp8yA
    bif_lookup <- setNames(as.character(bif$DATAVALUE), bif$VARIABLE)
    get_bif <- function(variable, default = NA_character_) {
        is_missing <- (
            length(variable) != 1L ||
            is.na(variable) ||
            !(variable %in% names(bif_lookup))
        )

        if (is_missing) {
            issues$invalid_key <<- c(issues$invalid_key, variable)
            return(default)
        }

        value <- bif_lookup[[variable]]

        if (is.na(value)) {
            issues$key_with_na <<- c(issues$key_with_na, variable)
            return(default)
        }

        value
    }

    ## TERN network contains only Australia or New Zealand sites
    country <- ifelse(
        grepl("^TERN_AU-", get_bif("PRODUCT_NAME")),
        "Australia",
               ifelse(
                   grepl("^TERN_NZ-", get_bif("PRODUCT_NAME")),
                   "New Zealand",
                   NA_character_
               )
    )

    ## Soil temp may exist independently of soil moisture. Assume bifvarinfo
    ## only available with the FLUXNET2015 FULLSET datasets
    dataset_vars <- get_varnames('FLUXNET2015', 'FULLSET', FALSE)
    soilt <- dataset_vars$soilt

    soil_heights <- instrument_height_string(bifvarinfo, soilt)
    if (length(soil_heights) == 0) {
        soilm <- dataset_vars$soilm
        soil_heights <- instrument_height_string(bifvarinfo, soilm)
    }

    ## Canopy/vegetation height is not always present. There may be multiple values
    ## (for different species). We will take the maximum value as the canopy height
    heightc <- unlist(
        bif_lookup[names(bif_lookup) == "HEIGHTC"],
        use.names = FALSE
    )
    max_heightc <- if (length(heightc) == 0L || all(is.na(heightc))) {
                       NA_real_
                   } else {
                       as.numeric(max(heightc, na.rm = TRUE))
                   }

    if (is.na(max_heightc)) {
        warnings <- append(
            warnings,
            list("Variable 'HEIGHTC' was not found; using default")
        )
    }

    height <- calc_instrument_height(bifvarinfo)

    if (is.na(height)) {
        warnings <- append(
            warnings,
            list("Variable 'VAR_INFO_HEIGHT' was not found for either 'VPD_F' or 'RH'; using default")
        )
    }

    list(
        site = list(
            Fullname = get_bif("SITE_NAME"),
            SiteLatitude = as.numeric(get_bif("LOCATION_LAT")),
            SiteLongitude = as.numeric(get_bif("LOCATION_LONG")),
            SiteElevation = as.numeric(get_bif("LOCATION_ELEV")),

            IGBP_vegetation_short = get_bif("IGBP"),
            IGBP_vegetation_long = get_bif("IGBP_COMMENT"),
            VegetationDescription = get_bif("IGBP_COMMENT"),
            CanopyHeight = max_heightc,
            Vegetation_height = max_heightc,

            Description = get_bif("SITE_DESC"),
            Disturbance = get_bif("DOM_DIST_MGMT"),
            CropDescription = get_bif("SITE_DESC"),

            Country = country,
            MeasurementHeight = height,
            SoilMeasurementHeight = soil_heights,

            Source = "TERN",
            Reference = get_bif("DOI_CITATION"),
            ACKNOWLEDGEMENT = get_bif("ACKNOWLEDGEMENT"),

            MAT = as.numeric(get_bif("MAT")),
            MAP = as.numeric(get_bif("MAP")),
            CLIMATE_KOEPPEN = get_bif("CLIMATE_KOEPPEN"),


            DOI_CONTRIBUTOR_NAME = get_bif("TEAM_MEMBER_NAME"),
            DOI_CONTRIBUTOR_EMAIL = get_bif("TEAM_MEMBER_EMAIL"),
            DOI_CONTRIBUTOR_INSTITUTION = get_bif("TEAM_MEMBER_INSTITUTION"),

            UTC_OFFSET = get_bif("UTC_OFFSET")
        ),
        warnings = warnings
    )
}

## Adaptor to convert ICOS sites into standard metadata object
## parmams: bif and bifvarinfo are dataframes
## returns: list(site, warning)
parse_icos <- function(bif, bifvarinfo, site_code) {
    if (nrow(bif) == 0L || nrow(bifvarinfo) == 0L) {
        return(list(site = uest(), variables = list(), warnings =
                                                           paste("Empty bif metadata for", site_code)))
    }

    ## Check first entry since all rows have the same site ID
    if (bif$SITE_ID[1] != site_code)    {
        return(list(site = list(), variables = list(), warnings =
                                                           paste("Site code doesn't match", site_code)))
    }

    warnings <- character()
    ## Lookup variable to attempt to source particular variables
    ## docs.google.com/spreadsheets/d/167CMz_39hEUZziO66yhfe5-4wabqlqUGpB19aeDp8yA
    bif_lookup <- setNames(as.character(bif$DATAVALUE), bif$VARIABLE)
    get_bif <- function(variable, default = NA_character_) {
        missing_variable <- (
            length(variable) != 1L ||
            is.na(variable) ||
            !(variable %in% names(bif_lookup))
        )

        if (missing_variable) {
            warnings <<- append(
                warnings,
                list(sprintf("Variable '%s' was not found; using default",
                            variable))
            )
            return(default)
        }

        value <- bif_lookup[[variable]]

        if (is.na(value)) {
            warnings <<- append(
                warnings,
                list(sprintf("Variable '%s' has an NA value; using default",
                            variable))
            )
            return(default)
        }

        value
    }

    ## Canopy/vegetation height is not always present. There may be multiple values
    ## (for different species). We will take the maximum value as the canopy height
    heightc <- unlist(
        bif_lookup[names(bif_lookup) == "HEIGHTC"],
        use.names = FALSE
    )
    max_heightc <- if (length(heightc) == 0L || all(is.na(heightc))) {
                       NA_real_
                   } else {
                       max(heightc, na.rm = TRUE)
                   }

    if (is.na(max_heightc)) {
        warnings <- append(
            warnings,
            list("Variable 'HEIGHTC' was not found; using default")
        )
    }

    height <- calc_instrument_height(bifvarinfo)

    if (is.na(height)) {
        warnings <- append(
            warnings,
            list("Variable 'VAR_INFO_HEIGHT' was not found for either 'VPD_F' or 'RH'; using default")
        )
    }

    ## Soil temp may exist independently of soil moisture. Assume bifvarinfo
    ## only available with the FLUXNET2015 FULLSET datasets
    dataset_vars <- get_varnames('FLUXNET2015', 'FULLSET', FALSE)
    soilt <- dataset_vars$soilt

    soil_heights <- instrument_height_string(bifvarinfo, soilt)
    if (length(soil_heights) == 0) {
        soilm <- dataset_vars$soilm
        soil_heights <- instrument_height_string(bifvarinfo, soilm)
    }

    list(
        site = list(
            Fullname = get_bif("SITE_NAME"),
            SiteLatitude = as.numeric(get_bif("LOCATION_LAT")),
            SiteLongitude = as.numeric(get_bif("LOCATION_LONG")),
            SiteElevation = as.numeric(get_bif("LOCATION_ELEV")),

            IGBP_vegetation_short = get_bif("IGBP"),
            IGBP_vegetation_long = get_bif("IGBP_COMMENT"),
            VegetationDescription = get_bif("IGBP_COMMENT"),
            CanopyHeight = max_heightc,
            Vegetation_height = max_heightc,

            Description = get_bif("SITE_DESC"),
            Disturbance = get_bif("DOM_DIST_MGMT"),
            CropDescription = get_bif("SITE_DESC"),

            Country = get_bif("COUNTRY"), # SAEON doesn't have

            MeasurementHeight = height,
            SoilMeasurementHeight = soil_heights,

            Source = get_bif("NETWORK"),
            Reference = get_bif("DOI_CITATION"),
            ACKNOWLEDGEMENT = get_bif("ACKNOWLEDGEMENT"),

            MAT = as.numeric(get_bif("MAT")),
            MAP = as.numeric(get_bif("MAP")),
            CLIMATE_KOEPPEN = get_bif("CLIMATE_KOEPPEN"),

            DOI_CONTRIBUTOR_NAME = get_bif("TEAM_MEMBER_NAME"),
            DOI_CONTRIBUTOR_EMAIL = get_bif("TEAM_MEMBER_EMAIL"),
            DOI_CONTRIBUTOR_INSTITUTION = get_bif("TEAM_MEMBER_INSTITUTION"),

            UTC_OFFSET = get_bif("UTC_OFFSET")
        ),
        warnings = warnings
    )
}

read_fluxnet_csv <- function(path) {
    if (length(path) != 1L || is.na(path) || !file.exists(path)) {
        return(NULL)
    }

    read.csv(
        path,
        header = TRUE,
        stringsAsFactors = FALSE,
        na.strings = c("", "NA", "N/A", "None", "-9999", "-9999.0")
    )
}

## From the bifvarinfo file, determine the datahub source - TERN, ICOS, AMF
detect_bif_hub <- function(bif, bifvarinfo) {
    processing_center <- bifvarinfo[
        toupper(trimws(as.character(bifvarinfo$VARIABLE))) ==
            "PRODUCT_PROCESSING_CENTER",
        "DATAVALUE"
    ]

    hub_map <- c(
        TERN = "TERN",
        EUDB_ICOS = "ICOS",
        AMP = "AMF"
    )

    unname(hub_map[processing_center])
}

## datahub registry to support different formats
datahub_adapters <- list(
    AMF = parse_ameriflux,
    TERN = parse_tern,
    ICOS = parse_icos
    )

## Main entry function to extract metadata from bif files. Delegate processing to specific adaptor
get_site_metadata_bif <- function(site_code, meta_files, incl_processing = TRUE, bif_hub = NULL) {
    required <- c("bif", "bifvarinfo")
    if (is.list(meta_files) &&
        !all(required %in% names(meta_files))) {
        stop(
            "`meta_files` must be a named list containing: ",
            paste(required, collapse = ", ")
        )
    }

    bif <- read_fluxnet_csv(meta_files$bif)
    bifvarinfo <- read_fluxnet_csv(meta_files$bifvarinfo)

    if (is.null(bif) || is.null(bifvarinfo)) {
        return(list(
            out = character(),
            warn= "meta data files do not exist"
        ))
    }

    if (is.null(bif_hub)) {
        bif_hub <- detect_bif_hub(bif, bifvarinfo)
    }

    adapter <- datahub_adapters[[bif_hub]]

    if (is.null(adapter)) {
        stop("Unsupported BIF data hub in datahub adapters: ", bif_hub)
    }

    result <- adapter(
        bif = bif,
        bifvarinfo = bifvarinfo,
        site_code = site_code
    )

    result$source <- list(
        hub = bif_hub,
        bif = meta_files$bif,
        bifvarinfo = meta_files$bifvarinfo
    )

    if (incl_processing) {
        warnings <- ""
        ## for (error in result$warnings) {
        ##     warnings <- append_and_warn(error, warnings, call = FALSE)
        ## }
        return(list(out=result$site,warn=warnings))
    } else {
        return(result$site)
    }
}

################################################
# Main metadata functions
################################################

#' Get site metadata. Tries multiple methods to retrieve full metadata
#'
#' @return metadata list
#' @export
get_site_metadata <- function(
    site_code,
    meta_files = NA,
    incl_processing=TRUE,
    use_csv=TRUE,
    update_csv=FALSE,
    ...
) {
    #Initialise warnings
    warnings <- ""

    metadata <- site_metadata_template(site_code)

    if (use_csv) {
        metadata <- get_site_metadata_from_CSV(metadata, incl_processing=FALSE, ...)
    }

    if (any(!is.na(meta_files))) {
        ## named list with $out (site meta) and $warn (list of warnings)
        result <- get_site_metadata_bif(
            site_code = site_code,
            meta_files = meta_files,
        )

        metadata <- update_metadata(metadata, result$out)
        warnings <- result$warn
    }

    ## Commented-out as all web sources are currently out of date
    ## if (any(check_missing(metadata))) {
    ##     metadata <- get_site_metadata_web(metadata, incl_processing=FALSE)
    ## }

    warn_missing_metadata(metadata)

    if (incl_processing) {
        metadata <- add_processing_metadata(metadata)
        warnings <- append_and_warn(warn=metadata$warn, warnings, call=FALSE)
        metadata <- metadata$out
    }

    if (update_csv) {
        save_metadata_to_csv(metadata)
    }

    return(list(out=metadata,warn=warnings))
}