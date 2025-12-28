test_that("ProviderGroqDeveloper inherits batch support from ProviderOpenAI", {
  skip_if_not_installed("ellmer")

  ellmer_ns <- asNamespace("ellmer")

  # Create ProviderGroqDeveloper instance
  provider <- ProviderGroqDeveloper(
    name = "Groq",
    base_url = "https://api.groq.com/openai/v1",
    model = "openai/gpt-oss-20b",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  # Check that it has batch support
  expect_true(ellmer_ns$has_batch_support(provider))
})

test_that("ProviderOpenAI has batch support", {
  skip_if_not_installed("ellmer")

  ellmer_ns <- asNamespace("ellmer")

  # Create a ProviderOpenAI instance for comparison
  provider_oa <- ellmer_ns$ProviderOpenAI(
    name = "Test",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  # Verify ProviderOpenAI has batch support
  expect_true(ellmer_ns$has_batch_support(provider_oa))
})

test_that("chat_groq_developer() Chat object supports batch methods", {
  skip_if_not_installed("ellmer")

  chat <- chat_groq_developer(credentials = function() "dummy_key_for_testing")

  # Get the provider
  provider <- chat$get_provider()

  # Verify it has batch support
  ellmer_ns <- asNamespace("ellmer")
  expect_true(ellmer_ns$has_batch_support(provider))
})

test_that("ProviderGroqDeveloper has same batch support as its parent", {
  skip_if_not_installed("ellmer")

  ellmer_ns <- asNamespace("ellmer")

  # Create both provider types
  provider_groq <- ProviderGroqDeveloper(
    name = "Groq",
    base_url = "https://api.groq.com/openai/v1",
    model = "openai/gpt-oss-20b",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  provider_openai <- ellmer_ns$ProviderOpenAI(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  # Both should have batch support
  expect_true(ellmer_ns$has_batch_support(provider_groq))
  expect_true(ellmer_ns$has_batch_support(provider_openai))
  expect_equal(
    ellmer_ns$has_batch_support(provider_groq),
    ellmer_ns$has_batch_support(provider_openai)
  )
})
