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
          tavg = c(1,2,2,1)
        )
      )

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
     library(lubridate)
     date = seq(from=dmy("1/1/2000"), to=dmy("1/1/2010"), by="days")
    clim_data =
      as.data.frame(
        cbind(
          month = month(date),
          day   = day(date),
          year  = year(date),
          tavg = rep(1, length(date))
        )
      )
    idx=which(date=="2002-04-1")
    clim_data[idx,"tavg"]=40
    expect_that(spring_summary(clim_data, spring_months=c(4))$warmest_spring, equals(2002))
  }
)
