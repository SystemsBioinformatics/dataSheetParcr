# Test for parse_comment
test_that("parse_comment works correctly", {
  expect_equal(parse_comment("# This is a comment"), " This is a comment")
  expect_equal(parse_comment("Not a comment"), list())
  expect_equal(parse_comment(""), list())
})

# Test for parse_header
test_that("parse_header works correctly", {
  expect_equal(parse_header("> Header line"), "Header line")
  expect_equal(parse_header("Not a header"), list())
  expect_equal(parse_header(""), list())
})

# Test for parse_tsv_line
test_that("parse_tsv_line works correctly", {
  expect_equal(parse_tsv_line("value1\tvalue2\tvalue3"), c("value1", "value2", "value3"))
  expect_equal(parse_tsv_line("> Header line"), list())
  expect_equal(parse_tsv_line("# Comment line"), list())
  expect_equal(parse_tsv_line(""), list())
})

# Test for matrix_to_df
test_that("matrix_to_df works correctly", {
  matrix <- matrix(c("col1", "col2", "val1", "val2"), nrow = 2, byrow = TRUE)
  df <- matrix_to_df(matrix)
  expect_equal(ncol(df), 2)
  expect_equal(nrow(df), 1)
  expect_equal(colnames(df), c("col1", "col2"))
  expect_equal(df$col1, "val1")
  expect_equal(df$col2, "val2")
})

# Test for DataLine
test_that("DataLine works correctly", {
  line <- "value1\tvalue2\tvalue3"
  result <- DataLine()(line)
  expect_equal(result$L[[1]], c("value1", "value2", "value3"))
})

# Test for DataTable
test_that("DataTable works correctly", {
  lines <- c(
    "a\tb\t",
    "1\t2\t",
    "3\t4\t",
    "\t\t"
  )
  result <- DataTable()(lines)
  expect_equal(ncol(result$L[[1]]), 2)
  expect_equal(nrow(result$L[[1]]), 2)
  expect_equal(colnames(result$L[[1]]), c("a", "b"))
})

# Test for DataBlock
test_that("DataBlock works correctly", {
  block <- c(
    "> some name",
    "# some comment",
    "a\tb\t",
    "1\t2\t",
    "3\t4\t",
    "\t\t"
  )
  result <- DataBlock()(block)
  expect_equal(names(result$L), "some name")
  expect_equal(ncol(result$L$`some name`), 2)
  expect_equal(nrow(result$L$`some name`), 2)
})

# Test for DataSheet
test_that("DataSheet works correctly", {
  sheet <- c(
    "> some name",
    "# some comment",
    "a\tb",
    "1\t2",
    "3\t4",
    "\t",
    "# a comment in between\t",
    "> another name",
    "# another comment",
    "x\ty",
    "5\t6",
    "7\t8",
    "\t",
    "\t"
  )
  result <- DataSheet()(sheet)
  expect_equal(names(result$L), c("some name", "another name"))
  expect_equal(names(result$L$`some name`), c("a", "b"))
  expect_equal(names(result$L$`another name`), c("x", "y"))
})

# Test for EmptyLine
test_that("EmptyLine works correctly", {
  line1 <- "\t\t\t"
  result <- EmptyLine()(line1)
  expect_equal(result$L[[1]], line1)
  line2 <- "   \t  \t  \t   "
  result <- EmptyLine()(line2)
  expect_equal(result$L[[1]], line2)
  expect_equal(class(EmptyLine()("\t\t not empty")), "marker")
})

# Test for read_sheet
test_that("parse_sheet works correctly", {
  sheet <- c(
    "> some name",
    "# some comment",
    "a\tb",
    "1\t2",
    "3\t4",
    "\t",
    "# a comment in between\t",
    "> another name",
    "# another comment",
    "x\ty",
    "5\t6",
    "7\t8",
    "\t",
    "\t"
  )
  result <- parse_sheet(sheet)
  expect_equal(names(result), c("some name", "another name"))
  expect_equal(names(result$`some name`), c("a", "b"))
  expect_equal(names(result$`another name`), c("x", "y"))
})
