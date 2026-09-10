roosevelt_level <- marsFetchLevelData(mars, target_id = "274-4-1",
                                      ow_suffix = "CW1",start_date = '2000-01-01',
                                      end_date = '2030-01-01',
                                      sump_correct = FALSE)

roosevelt_years <- mutate(roosevelt_level, year = year(dtime), month = month(dtime))

