test_that("ProviderGroqDeveloper class is properly defined", {
  skip_if_not_installed("ellmer")

  # Check that the class exists and is an S7 class
  expect_true(inherits(ProviderGroqDeveloper, "S7_class"))

  # Check that it extends ProviderOpenAI
  parent_class <- attr(ProviderGroqDeveloper, "parent")
  expect_equal(attr(parent_class, "name"), "ProviderOpenAI")
})

test_that("chat_groq_developer creates a valid Chat object", {
  skip_if_not_installed("ellmer")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "GROQ_API_KEY not set")

  chat <- chat_groq_developer()

  # Check it's a Chat object
  expect_s3_class(chat, "Chat")

  # Check provider is ProviderGroqDeveloper
  provider <- chat$get_provider()
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("structured output schema has additionalProperties: false", {
  skip_if_not_installed("ellmer")

  # Create a simple type
  ellmer_ns <- asNamespace("ellmer")
  type_obj <- ellmer_ns$type_object(
    name = ellmer_ns$type_string(),
    age = ellmer_ns$type_integer()
  )

  # Create provider
  provider <- ProviderGroqDeveloper(
    name = "Groq",
    base_url = "https://api.groq.com/openai/v1",
    model = "openai/gpt-oss-20b",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test"),
    extra_headers = character()
  )

  # Convert to JSON schema
  schema <- ellmer_ns$as_json(provider, type_obj)

  # Check that additionalProperties is false
  expect_equal(schema$additionalProperties, FALSE)
  expect_equal(schema$type, "object")
})
