#' Chat with Groq AI Models (Developer Version)
#'
#' Creates a chat interface for Groq's API with support for structured outputs,
#' batch processing, and parallel execution. This developer version extends
#' ProviderOpenAI to inherit full batch support while adding Groq-specific
#' schema formatting (additionalProperties: false).
#'
#' @param system_prompt Optional system prompt to set context for the conversation
#' @param base_url Base URL for Groq API. Defaults to Groq's OpenAI-compatible endpoint
#' @param api_key Deprecated. Use `credentials` instead
#' @param credentials API credentials. Defaults to `GROQ_API_KEY` environment variable
#' @param model Model to use. Defaults to "openai/gpt-oss-20b" which supports
#'   strict structured outputs. Also available: "openai/gpt-oss-120b"
#' @param params Optional params object for controlling generation
#' @param api_args Additional arguments passed to the API
#' @param echo How to display output. One of "none", "text", "all"
#' @param api_headers Additional HTTP headers
#'
#' @return A Chat object with methods for:
#'   - `chat()`: Send messages and receive responses
#'   - `chat_structured()`: Extract structured data with type validation
#'   - Batch processing via ellmer's `batch_chat()` and `batch_chat_structured()`
#'   - Parallel processing via ellmer's `parallel_chat()` and `parallel_chat_structured()`
#'
#' @section Structured Outputs:
#' Groq supports strict JSON schema validation with guaranteed compliance when using
#' compatible models. All objects must have `additionalProperties: false` and all
#' fields must be required (or use union with null for optional fields).
#'
#' @section Batch Processing:
#' Groq's batch API offers 50% cost discount and no rate limit impact. Batch jobs
#' are processed asynchronously with completion windows from 24 hours to 7 days.
#' Use `batch_chat()` or `batch_chat_structured()` from ellmer to submit batches.
#'
#' @section Models:
#' For strict structured output support, use:
#' - `"openai/gpt-oss-20b"` (default, recommended)
#' - `"openai/gpt-oss-120b"`
#'
#' Other models support best-effort JSON output but may not guarantee schema compliance.
#'
#' @examples
#' \dontrun{
#' # Basic chat
#' chat <- chat_groq_developer()
#' chat$chat("What is the capital of France?")
#'
#' # Structured output
#' type_person <- type_object(
#'   name = type_string(),
#'   age = type_integer(),
#'   city = type_string()
#' )
#' result <- chat$chat_structured(
#'   "John is 30 years old and lives in NYC",
#'   type = type_person
#' )
#'
#' # Batch processing (requires ellmer's batch functions)
#' library(ellmer)
#' chat <- chat_groq_developer()
#' results <- batch_chat(
#'   chat,
#'   prompts = c("Question 1?", "Question 2?", "Question 3?"),
#'   wait = FALSE  # Async - check back later
#' )
#'
#' # Parallel processing
#' results <- parallel_chat(
#'   chat,
#'   prompts = c("Question 1?", "Question 2?", "Question 3?")
#' )
#' }
#'
#' @seealso
#' - [ellmer::batch_chat()] for batch processing
#' - [ellmer::parallel_chat()] for parallel processing
#' - [ellmer::type_object()], [ellmer::type_array()] for defining types
#'
#' @export
chat_groq_developer <- function(
  system_prompt = NULL,
  base_url = "https://api.groq.com/openai/v1",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  # Access ellmer's internal functions
  ellmer_ns <- asNamespace("ellmer")

  # Default to a model that supports strict structured outputs
  model <- model %||% "openai/gpt-oss-20b"

  # Handle credentials
  if (is.null(credentials)) {
    if (!is.null(api_key)) {
      lifecycle::deprecate_warn(
        "0.1.0",
        "chat_groq_developer(api_key)",
        "chat_groq_developer(credentials)"
      )
      credentials <- api_key
    } else {
      # Get from environment variable
      api_key <- Sys.getenv("GROQ_API_KEY")
      if (api_key == "") {
        cli::cli_abort(c(
          "Groq API key not found.",
          "i" = "Set {.envvar GROQ_API_KEY} or provide {.arg credentials}"
        ))
      }
    }
  }

  # Create credentials object
  credentials <- ellmer_ns$as_credentials(
    "chat_groq_developer",
    function() Sys.getenv("GROQ_API_KEY"),
    credentials = credentials
  )

  if (is.null(params)) {
    params <- ellmer_ns$params()
  }

  if (is.null(echo)) {
    echo <- ellmer_ns$check_echo("none")
  }

  # Create ProviderGroqDeveloper instance
  provider <- ProviderGroqDeveloper(
    name = "Groq",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )

  # Create and return Chat object
  ellmer_ns$Chat$new(
    provider = provider,
    system_prompt = system_prompt,
    echo = echo
  )
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x
