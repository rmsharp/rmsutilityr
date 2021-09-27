context("numbers2words")
test_that("numbers2words forms the correct expression", {
  num_1 <- 1
  num_12 <- 12
  num_18 <- 18
  num_123 <- 123 
  num_0 <- 0
  num_0.1 <- 0.1
  num_0.499 <- 0.499
  num_0.5 <- 0.5
  num_minus_1 <- -1 
  num_1.2 <- 1.2
  num_1.499 <- 1.499
  num_1.5 <- 1.5
  num_1.55 <- 1.55
  num_minus_0.6 <- -0.6 
  num_10 <- 10
  num_20 <- 20
  num_30 <- 30
  num_40 <- 40
  num_100 <- 100
  num_200 <- 200
  num_300 <- 300
  num_400 <- 400
  num_211 <- 211
  num_210 <- 210
  num_201 <- 201
  num_minus_211 <- -211
  num_minus_210 <- -210
  num_543210 <- 543210
  num_minus_543210 <- -543210
  num_9876543210 <- 9876543210
  num_98765432109876543210 <- 98765432109876543210
  expect_equal(numbers2words(num_1), "one")
  expect_equal(numbers2words(num_12), "twelve")
  expect_equal(numbers2words(num_18), "eighteen")
  expect_equal(numbers2words(num_123), "one hundred twenty three")
  expect_equal(numbers2words(num_0), "zero")
  expect_equal(numbers2words(num_0.1), "zero")
  expect_equal(numbers2words(num_0.499), "zero")
  expect_equal(numbers2words(num_0.5), "zero")
  expect_equal(numbers2words(num_minus_1), "negative one")
  expect_equal(numbers2words(num_1.2), "one")
  expect_equal(numbers2words(num_1.499), "one")
  expect_equal(numbers2words(num_1.5), "two")
  expect_equal(numbers2words(num_1.55), "two")
  expect_equal(numbers2words(num_minus_0.6), "negative one")
  expect_equal(numbers2words(num_10), "ten")
  expect_equal(numbers2words(num_20), "twenty")
  expect_equal(numbers2words(num_30), "thirty")
  expect_equal(numbers2words(num_40), "forty")
  expect_equal(numbers2words(num_100), "one hundred")
  expect_equal(numbers2words(num_200), "two hundred")
  expect_equal(numbers2words(num_300), "three hundred")
  expect_equal(numbers2words(num_400), "four hundred")
  expect_equal(numbers2words(num_211), "two hundred eleven")
  expect_equal(numbers2words(num_210), "two hundred ten")
  expect_equal(numbers2words(num_201), "two hundred one")
  expect_equal(numbers2words(num_minus_211), "negative two hundred eleven")
  expect_equal(numbers2words(num_minus_210), "negative two hundred ten")
  expect_equal(numbers2words(num_543210), 
               "five hundred forty three thousand, two hundred ten")
  expect_equal(numbers2words(num_minus_543210), 
               "negative five hundred forty three thousand, two hundred ten")
  expect_equal(numbers2words(
    num_9876543210), 
    paste0("nine billion, eight hundred seventy six million, ", 
    "five hundred forty three thousand, two hundred ten"))
  expect_error(numbers2words(num_98765432109876543210), 
               "98765432109876543488 is too large!")
})

