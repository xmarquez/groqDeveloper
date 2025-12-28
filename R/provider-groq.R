#' Groq Developer Provider Class
#'
#' S7 class that extends ProviderOpenAI to provide Groq-specific functionality.
#' Inherits all batch processing capabilities from ProviderOpenAI while ensuring
#' proper schema formatting for Groq's strict JSON validation requirements.
#'
#' @param name Provider name
#' @param model Model identifier (e.g., "openai/gpt-oss-20b")
#' @param base_url API base URL
#' @param params Parameters list for generation control
#' @param extra_args Additional API arguments
#' @param extra_headers Additional HTTP headers
#' @param credentials API credentials (function or string)
#' @param service_tier Service tier specification
#'
#' @details
#' Users should typically use [chat_groq_developer()] instead of calling this
#' constructor directly. This class is exported for advanced use cases.
#'
#' @export
ProviderGroqDeveloper <- NULL

# Helper functions --------------------------------------------------------

#' Recursively add additionalProperties: false to all objects
#'
#' Groq requires additionalProperties: false on all objects for strict mode.
#' This helper ensures all nested objects have this property set.
#'
#' @param node A list representing a JSON schema node
#' @return The modified node with additionalProperties: false on all objects
#' @noRd
add_additional_properties_false <- function(node) {
  if (is.list(node) && !is.null(node$type) && identical(node$type, "object")) {
    node$additionalProperties <- FALSE
    if (!is.null(node$properties) && is.list(node$properties)) {
      node$properties <- lapply(node$properties, add_additional_properties_false)
    }
  }
  if (is.list(node) && !is.null(node$items)) {
    node$items <- add_additional_properties_false(node$items)
  }
  node
}

# Method registration -----------------------------------------------------

register_groq_methods <- function() {
  ellmer_ns <- asNamespace("ellmer")

  # Get the as_json generic from ellmer
  as_json <- S7::new_external_generic("ellmer", "as_json", "provider")

  # Get the actual chat_body generic from ellmer's namespace
  chat_body <- ellmer_ns$chat_body

  # Get type classes
  TypeObject <- ellmer_ns$TypeObject
  TypeArray <- ellmer_ns$TypeArray

  # Override chat_body to remove 'include' field (not supported by Groq)
  # Get the parent class method for ProviderOpenAI
  parent_chat_body <- S7::method(chat_body, ellmer_ns$ProviderOpenAI)

  S7::method(chat_body, ProviderGroqDeveloper) <- function(
    provider,
    stream = TRUE,
    turns = list(),
    tools = list(),
    type = NULL
  ) {
    # Call parent implementation
    body <- parent_chat_body(
      provider,
      stream = stream,
      turns = turns,
      tools = tools,
      type = type
    )

    # Remove 'include' field - Groq API doesn't support it
    body$include <- NULL

    body
  }

  # Register TypeObject method
  S7::method(as_json, list(ProviderGroqDeveloper, TypeObject)) <- function(provider, x, ...) {
    # Validate that additional_properties is not set (Groq doesn't support it)
    if (S7::prop(x, "additional_properties")) {
      cli::cli_abort("{.arg additional_properties} not supported for Groq structured outputs.")
    }

    # Get required properties
    props <- S7::prop(x, "properties")
    required <- purrr::map_lgl(props, function(prop) S7::prop(prop, "required"))

    # Build schema
    schema <- purrr::compact(list(
      type = "object",
      description = S7::prop(x, "description"),
      properties = ellmer_ns$as_json(provider, props, ...),
      required = as.list(names(props)[required]),
      additionalProperties = FALSE
    ))

    # Recursively ensure all nested objects have additionalProperties: false
    add_additional_properties_false(schema)
  }

  # Register TypeArray method
  S7::method(as_json, list(ProviderGroqDeveloper, TypeArray)) <- function(provider, x, ...) {
    items <- ellmer_ns$as_json(provider, S7::prop(x, "items"), ...)
    schema <- purrr::compact(list(
      type = "array",
      description = S7::prop(x, "description"),
      items = items
    ))

    add_additional_properties_false(schema)
  }
}

# Class initialization ----------------------------------------------------

# Initialize the class after ellmer is loaded
.onLoad <- function(libname, pkgname) {
  tryCatch({
    # Get ProviderOpenAI from ellmer
    ellmer_ns <- asNamespace("ellmer")
    ProviderOpenAI <- ellmer_ns$ProviderOpenAI

    # Create ProviderGroqDeveloper class extending ProviderOpenAI
    ProviderGroqDeveloper <<- S7::new_class(
      name = "ProviderGroqDeveloper",
      package = "groqDeveloper",
      parent = ProviderOpenAI
    )

    # Register method overrides
    register_groq_methods()

    # Register all S7 methods with ellmer
    S7::methods_register()
  }, error = function(e) {
    # During documentation build, ellmer may not be available
    # Suppress errors and rely on lazy loading
    NULL
  })
}

# Batch support is inherited from ProviderOpenAI ---------------------------
# No need to override batch methods - they work as-is with Groq's API
