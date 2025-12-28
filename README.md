# groqDeveloper

<!-- badges: start -->
<!-- badges: end -->

Provides a Groq AI provider for the [ellmer](https://github.com/tidyverse/ellmer) package with full support for structured outputs, batch processing, and parallel execution.

## Features

- **Structured Outputs**: Extract data with strict JSON schema validation
- **Batch Processing**: 50% cost discount with Groq's batch API
- **Parallel Execution**: Process multiple prompts concurrently
- **Full ellmer Integration**: Works seamlessly with `batch_chat()`, `parallel_chat()`, etc.

## Installation

You can install the development version of groqDeveloper from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/groqDeveloper")
```

## Setup

Set your Groq API key:

```r
Sys.setenv(GROQ_API_KEY = "your-api-key-here")
```

Or pass it directly:

```r
chat <- chat_groq(credentials = "your-api-key")
```

## Examples

### Basic Chat

```r
library(groqDeveloper)
library(ellmer)

chat <- chat_groq_developer()
chat$chat("What is the capital of France?")
```

### Structured Output

```r
# Define the structure
type_person <- type_object(
  name = type_string(),
  age = type_integer(),
  city = type_string()
)

# Extract structured data
chat <- chat_groq_developer()
result <- chat$chat_structured(
  "John is 30 years old and lives in New York City",
  type = type_person
)

# Result is a list matching the schema
str(result)
#> List of 3
#>  $ name: chr "John"
#>  $ age : int 30
#>  $ city: chr "New York City"
```

### Batch Processing

Process multiple requests with 50% cost savings:

```r
library(ellmer)

chat <- chat_groq_developer()

# Submit batch job
results <- batch_chat(
  chat,
  prompts = c(
    "What is 2+2?",
    "What is the capital of France?",
    "Who wrote Romeo and Juliet?"
  ),
  wait = FALSE  # Don't wait for completion
)

# Check status later
if (batch_chat_completed(results)) {
  data <- batch_chat_retrieve(results)
}
```

### Batch Structured Output

```r
type_answer <- type_object(
  question = type_string(),
  answer = type_string()
)

results <- batch_chat_structured(
  chat,
  prompts = c("What is 2+2?", "What is the capital of France?"),
  type = type_answer,
  wait = FALSE
)
```

### Parallel Processing

Process multiple prompts concurrently (synchronous):

```r
results <- parallel_chat(
  chat,
  prompts = c(
    "Tell me about Paris",
    "Tell me about London",
    "Tell me about Tokyo"
  )
)
```

## Supported Models

For strict structured outputs, use:
- `openai/gpt-oss-20b` (default, recommended)
- `openai/gpt-oss-120b`

Other models support best-effort JSON but may not guarantee schema compliance.

## How It Works

groqDeveloper extends `ProviderOpenAI` from ellmer, inheriting all batch processing capabilities while adding Groq-specific enhancements:

1. **Automatic Schema Formatting**: Adds `additionalProperties: false` to all objects (required by Groq)
2. **Strict Validation**: Ensures schemas meet Groq's requirements
3. **OpenAI Compatibility**: Leverages Groq's OpenAI-compatible API

## Benefits

- **Cost Effective**: 50% discount on batch processing
- **No Rate Limits**: Batch requests don't count toward rate limits
- **Type Safety**: Strict JSON schema validation ensures data quality
- **Easy Integration**: Works with all ellmer features out of the box

## License

MIT © groqDeveloper authors
