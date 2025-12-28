#' Groq Developer Provider Class
#'
#' S7 class that extends ProviderOpenAICompatible to provide Groq-specific functionality.
#' Inherits all capabilities from ProviderOpenAICompatible (which uses the Chat Completions
#' API format) while ensuring proper schema formatting for Groq's strict JSON validation.
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

  # Get type classes
  TypeObject <- ellmer_ns$TypeObject
  TypeArray <- ellmer_ns$TypeArray

  # Note: No need to override chat_body since ProviderOpenAICompatible

  # already uses the Chat Completions API format that Groq supports.
  # We only need to override as_json methods for TypeObject and TypeArray
  # to add additionalProperties: false for Groq's strict JSON validation.

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
    # Get ProviderOpenAICompatible from ellmer (NOT ProviderOpenAI)
    # ProviderOpenAI uses the Responses API format which Groq doesn't support
    # ProviderOpenAICompatible uses the Chat Completions API format
    ellmer_ns <- asNamespace("ellmer")
    ProviderOpenAICompatible <- ellmer_ns$ProviderOpenAICompatible

    # Create ProviderGroqDeveloper class extending ProviderOpenAICompatible
    ProviderGroqDeveloper <<- S7::new_class(
      name = "ProviderGroqDeveloper",
      package = "groqDeveloper",
      parent = ProviderOpenAICompatible
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

# Capabilities are inherited from ProviderOpenAICompatible ------------------
# No need to override batch/parallel methods - they work as-is with Groq's API
