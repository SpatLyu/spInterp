test_that("deg2rad works", {
  expect_equal(deg2rad(180) %>% rad2deg(), 180)

  # North: 0
  # East: 90
  # South: 180
  # West:
  expect_equal(angle(
    data.frame(x = 1, y = 0),
    data.frame(x = 0, y = 1)
  ) %>% rad2deg(), 90)
  
  expect_equal(angle(
    c(1, 0), data.frame(x = 0, y = 1)
  ) %>% rad2deg(), 90)
  
  expect_equal(angle(c(1, 0), c(0, 1)) %>% rad2deg(), 90)

  expect_equal(angle(
    data.frame(x = 0, y = 1),
    data.frame(x = 1, y = 0)
  ) %>% rad2deg(), 90)

  expect_equal(angle(
    data.frame(x = 0, y = 1),
    data.frame(x = -1, y = 0)
  ) %>% rad2deg(), 90)
})
