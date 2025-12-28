test_that("ProviderGroqDeveloper class structure is correct", {
  skip_if_not_installed("ellmer")

  # Check that the class exists and is an S7 class
  expect_true(inherits(ProviderGroqDeveloper, "S7_class"))

  # Check that it extends ProviderOpenAI
  parent_class <- attr(ProviderGroqDeveloper, "parent")
  expect_equal(attr(parent_class, "name"), "ProviderOpenAI")
})

test_that("Direct ProviderGroqDeveloper creation works", {
  skip_if_not_installed("ellmer")

  ellmer_ns <- asNamespace("ellmer")
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
  expect_true(ellmer_ns$has_batch_support(provider))
})

test_that("Schema generation adds additionalProperties: false", {
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

  # Create a simple type
  type_obj <- ellmer_ns$type_object(
    name = ellmer_ns$type_string(),
    age = ellmer_ns$type_integer()
  )

  # Convert to JSON schema
  schema <- ellmer_ns$as_json(provider, type_obj)

  # Verify schema properties
  expect_equal(schema$type, "object")
  expect_equal(schema$additionalProperties, FALSE)
  expect_true("name" %in% names(schema$properties))
  expect_true("age" %in% names(schema$properties))
})

test_that("Nested object schema has recursive additionalProperties: false", {
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

  # Create nested type
  type_nested <- ellmer_ns$type_object(
    person = ellmer_ns$type_object(
      name = ellmer_ns$type_string(),
      age = ellmer_ns$type_integer()
    ),
    address = ellmer_ns$type_object(
      city = ellmer_ns$type_string(),
      country = ellmer_ns$type_string()
    )
  )

  # Convert to JSON schema
  schema_nested <- ellmer_ns$as_json(provider, type_nested)

  # Verify top level
  expect_equal(schema_nested$additionalProperties, FALSE)

  # Verify nested objects
  expect_equal(schema_nested$properties$person$additionalProperties, FALSE)
  expect_equal(schema_nested$properties$address$additionalProperties, FALSE)
})

test_that("Array schema has additionalProperties: false on items", {
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

  # Create array type with object items
  type_arr <- ellmer_ns$type_array(
    ellmer_ns$type_object(name = ellmer_ns$type_string())
  )

  # Convert to JSON schema
  schema_arr <- ellmer_ns$as_json(provider, type_arr)

  # Verify array and items
  expect_equal(schema_arr$type, "array")
  expect_equal(schema_arr$items$type, "object")
  expect_equal(schema_arr$items$additionalProperties, FALSE)
})
