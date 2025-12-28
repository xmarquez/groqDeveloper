test_that("chat_groq_developer() creates a valid Chat object", {
  skip_if_not_installed("ellmer")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "GROQ_API_KEY not set")

  chat <- chat_groq_developer()

  # Check it's a Chat object
  expect_s3_class(chat, "Chat")

  # Check provider is ProviderGroqDeveloper
  provider <- chat$get_provider()
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("chat_groq_developer() works with explicit credentials", {
  skip_if_not_installed("ellmer")

  # Should work without error even with dummy credentials
  chat <- chat_groq_developer(credentials = function() "dummy_key_for_testing")

  expect_s3_class(chat, "Chat")

  provider <- chat$get_provider()
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("structured output extraction works with simple types", {
  skip_if_not_installed("ellmer")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "GROQ_API_KEY not set")

  ellmer_ns <- asNamespace("ellmer")
  chat <- chat_groq_developer()

  type_person <- ellmer_ns$type_object(
    name = ellmer_ns$type_string(),
    age = ellmer_ns$type_integer(),
    city = ellmer_ns$type_string()
  )

  result <- chat$chat_structured(
    "John is 30 years old and lives in New York City",
    type = type_person
  )

  # Verify result structure
  expect_true(is.list(result))
  expect_true("name" %in% names(result))
  expect_true("age" %in% names(result))
  expect_true("city" %in% names(result))
})

test_that("structured output extraction works with arrays of objects", {
  skip_if_not_installed("ellmer")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "GROQ_API_KEY not set")

  ellmer_ns <- asNamespace("ellmer")
  chat <- chat_groq_developer()

  type_people <- ellmer_ns$type_array(
    ellmer_ns$type_object(
      name = ellmer_ns$type_string(),
      age = ellmer_ns$type_integer(),
      height = ellmer_ns$type_number("in m"),
      weight = ellmer_ns$type_number("in kg")
    )
  )

  prompt <- r"(
 * John Smith. Age: 30. Height: 180 cm. Weight: 80 kg.
 * Jane Doe. Age: 25. Height: 5'5". Weight: 110 lb.
 * Jose Rodriguez. Age: 40. Height: 190 cm. Weight: 90 kg.
 * June Lee | Age: 35 | Height 175 cm | Weight: 70 kg
)"

  result <- chat$chat_structured(prompt, type = type_people)

  # Verify result structure
  expect_true(is.list(result))
  expect_true(length(result) > 0)

  # Check first entry has expected fields
  if (length(result) > 0) {
    expect_true("name" %in% names(result))
    expect_true("age" %in% names(result))
    expect_true("height" %in% names(result))
    expect_true("weight" %in% names(result))
  }
})

test_that("chat_groq_developer() supports system prompts", {
  skip_if_not_installed("ellmer")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "GROQ_API_KEY not set")

  chat <- chat_groq_developer(
    system_prompt = "You are a helpful assistant that responds concisely."
  )

  expect_s3_class(chat, "Chat")

  provider <- chat$get_provider()
  expect_true(S7::S7_inherits(provider, ProviderGroqDeveloper))
})

test_that("chat_groq_developer() allows model selection", {
  skip_if_not_installed("ellmer")

  # Test with different model
  chat <- chat_groq_developer(
    model = "openai/gpt-oss-120b",
    credentials = function() "dummy_key_for_testing"
  )

  expect_s3_class(chat, "Chat")
  expect_equal(chat$get_model(), "openai/gpt-oss-120b")
})
