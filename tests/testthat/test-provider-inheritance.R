test_that("ProviderGroqDeveloper inherits from ProviderOpenAICompatible", {
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

  # Check that it's a valid ProviderGroqDeveloper
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))

  # Check parent class is ProviderOpenAICompatible
  parent_class <- attr(ProviderGroqDeveloper, "parent")
  expect_equal(attr(parent_class, "name"), "ProviderOpenAICompatible")
})

test_that("chat_groq_developer() creates valid Chat object", {
  skip_if_not_installed("ellmer")

  chat <- chat_groq_developer(credentials = function() "dummy_key_for_testing")

  # Get the provider
  provider <- chat$get_provider()

  # Verify it's a ProviderGroqDeveloper
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("ProviderGroqDeveloper can be created with default settings", {
  skip_if_not_installed("ellmer")

  ellmer_ns <- asNamespace("ellmer")

  # Create provider
  provider <- ProviderGroqDeveloper(
    name = "Groq",
    base_url = "https://api.groq.com/openai/v1",
    model = "openai/gpt-oss-20b",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("ProviderGroqDeveloper extends ProviderOpenAICompatible correctly", {
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

  provider_compat <- ellmer_ns$ProviderOpenAICompatible(
    name = "Compatible",
    base_url = "https://api.example.com/v1",
    model = "test-model",
    params = ellmer_ns$params(),
    extra_args = list(),
    credentials = ellmer_ns$as_credentials("test", function() "test_key"),
    extra_headers = character()
  )

  # Both should be valid providers
  expect_true(S7::S7_inherits(provider_groq, ProviderGroqDeveloper))
  expect_true(S7::S7_inherits(provider_compat, ellmer_ns$ProviderOpenAICompatible))

  # ProviderGroqDeveloper should also be a ProviderOpenAICompatible
  expect_true(S7::S7_inherits(provider_groq, ellmer_ns$ProviderOpenAICompatible))
})
