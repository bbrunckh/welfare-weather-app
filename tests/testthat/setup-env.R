# Pin process-wide settings for the test run so a developer's ~/.Renviron (or a
# CI runner's environment) cannot change test behaviour, and so tests never read
# or write the real user cache. testthat restores everything at teardown; tests
# that need a different value still set it with withr::local_envvar().
.wise_test_root <- withr::local_tempdir(.local_envir = testthat::teardown_env())

withr::local_envvar(
  WISEAPP_WEATHER_CACHE_DIR = file.path(.wise_test_root, "weather-cache"),
  WISEAPP_PREPARED_WEATHER_CACHE_DIR = file.path(.wise_test_root, "prepared-weather-cache"),
  # Data-source selection and credentials (see AGENTS.md).
  WISEAPP_DATA_SOURCE = NA, WISEAPP_DATA_PATH = NA,
  DATABRICKS_HOST = NA, DATABRICKS_CLIENT_ID = NA, DATABRICKS_CLIENT_SECRET = NA,
  DATABRICKS_VOLUME_PATH = NA,
  S3_BUCKET = NA, S3_PREFIX = NA, S3_REGION = NA,
  AWS_ACCESS_KEY_ID = NA, AWS_SECRET_ACCESS_KEY = NA,
  GCS_BUCKET = NA, GCS_PREFIX = NA, GCS_ACCESS_KEY_ID = NA, GCS_SECRET_ACCESS_KEY = NA,
  AZURE_STORAGE_ACCOUNT = NA, AZURE_STORAGE_CONTAINER = NA, AZURE_STORAGE_PREFIX = NA,
  AZURE_STORAGE_KEY = NA, AZURE_CLIENT_ID = NA, AZURE_CLIENT_SECRET = NA,
  AZURE_TENANT_ID = NA,
  HF_REPO = NA, HF_SUBDIR = NA,
  .local_envir = testthat::teardown_env()
)
