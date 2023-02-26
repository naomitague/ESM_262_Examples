test_that(
  "spring_summary_averaging_works",
  {

    # Generate Climate Data where there is a single year with 4 months, and each month just has one day - and we can pick one to call it spring
    # Simple way to test because its 'easy' to see what the function should return if working correctly
    # in this sample data set there is only one year - but 4 different months, we can test spring selection
    clim_data =
      as.data.frame(
        cbind(
          month = c(1:4),
          day   = rep(1, times = 4),
          year  = rep(1, times = 4),
          precip  = c(0,0,0,1),
          tavg = c(1,2,2,1)
        )
      )

    expect_that(spring_summary(clim_data, spring_months=c(1))$Tavg_spring, equals(1))
    expect_that(spring_summary(clim_data, spring_months=c(1:4))$Tavg_spring, equals((1+2+2+1)/4))
  }
)

test_that(
  "spring_summary_extremes_works",
  {

    # Generate Climate Data where there is a single year with 4 months, and each month just has one day - and we can pick one to call it spring
    # Simple way to test because its 'easy' to see what the function should return if working correctly
    # same data set
    clim_data =
      as.data.frame(
        cbind(
          month = c(1:4),
          day   = rep(1, times = 4),
          year  = rep(1, times = 4),
          precip  = c(0,0,0,1),
          tavg = c(1,2,2,1)
        )
      )

    expect_that(spring_summary(clim_data, spring_months=4)$Pmax_spring, equals(1))
    expect_that(spring_summary(clim_data, spring_months=4)$Tmax_spring, equals(1))
    expect_that(spring_summary(clim_data, spring_months=c(1:4))$Tmax_spring, equals(2))

  }
)

# finally lets test whether multiple years works
test_that(
  "spring_summary_averaging_works_multiple_years",
  {

    # Generate Climate Data where there is a single year with 4 months, and each month just has one day - and we can pick one to call it spring
    # Simple way to test because its 'easy' to see what the function should return if working correctly
    # in this sample data set there is only one year - but 4 different months, we can test spring selection
    clim_data =
      as.data.frame(
        cbind(
          month = c(3,4,3,4),
          day   = rep(1, times = 4),
          year  = c(2001,2001,2002,2002),
          precip  = c(0,0,0,1),
          tavg = c(1,2,2,1)
        )
      )

    expect_that(spring_summary(clim_data, spring_months=c(4))$wettest_spring, equals(2002))
    expect_that(spring_summary(clim_data, spring_months=c(1:4))$Pmax_spring, equals(1))
  }
)
