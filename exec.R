library(nycflights13)
library(tidyverse)

glimpse(flights)

flights |> 
  filter(dep_delay > 120)

flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month == 1 | month == 2)

flights |> 
  filter(month %in% c(1, 2))

flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

arrange(flights, desc(dep_delay))

flights |> 
  distinct(origin, dest)


flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |> 
  count(origin, dest, sort = TRUE)

flights |> 
  count(origin, dest) |> 
  arrange(desc(n))

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  ) 

flights |> 
  select(year, month, day)

flights |> 
  select(year:day)

flights |> 
  select(!year:day)

flights |> 
  select(-(year:day))

flights |> 
  select(where(is.character))

flights |> 
  select(num_range("x", 1:3))

glimpse(flights)

flights |> 
  select(tail_num = tailnum)

flights |> 
  rename(tail_num = tailnum) |> 
  glimpse()

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour) |> 
  glimpse()

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )

flights |> 
  group_by(month) |> 
  count()

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |> 
  relocate(dest)

daily <- flights |> 
  group_by(year, month, day) 

glimpse(daily)

daily_flights <- daily |> 
  summarize(n = n())

glimpse(daily_flights)

daily <- ungroup(daily)

daily


flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  ) |> 
  arrange(month)

table1 |> 
  mutate(rate = cases / population * 10000)

table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases))

ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 and 2000

table4a

table4a |> 
  pivot_longer(
    cols = c(`1999`, `2000`), 
    names_to = "year",
    values_to = "cases"
    )

glimpse(billboard)

billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()

cms_patient_experience

cms_patient_experience |> 
  pivot_wider(
    id_cols = c(org_pac_id, org_nm),
    names_from = measure_cd,
    values_from = prf_rate
  )


students <- read_csv("data/students.csv", na = c("N/A", ""))

students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  ) |> 
  mutate(meal_plan = factor(mealPlan), .keep = "unused")

students <- read_csv("https://pos.it/r4ds-students-csv")

tibble(
  x = c(1, 2, 5),
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)

diamonds |> 
  ggplot(aes(x = cut)) +
  geom_bar()

diamonds |> 
  count(cut) |> 
  ggplot(aes(x = cut, y = n)) +
  geom_col()

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

diamonds |> 
  ggplot(aes(x = cut)) +
  geom_bar()

diamonds |> 
  ggplot(aes(x = cut)) +
  geom_bar(stat = "count")

diamonds |> 
  ggplot(aes(x = cut)) +
  stat_count()

diamonds |> 
  count(cut) |> 
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

diamonds |> 
  count(cut) |> 
  ggplot(aes(x = cut, y = n)) +
  geom_col()

diamonds |> 
  ggplot(aes(x = cut, y = after_stat(prop), group = 1)) +
  geom_bar()

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |> 
  arrange(y)

library(gapminder)
glimpse(gapminder)

View(gapminder)

gapminder |> 
  filter(year == 1952) |> 
  summarize(
    pop_cont = sum(pop),
    .by = continent 
  )

gapminder |> 
  filter(year == 1952) |> 
  mutate(
    pop_cont = sum(pop),
    .by = continent
  ) |> 
  mutate(
    pop_cont_per = pop * 100 / pop_cont
  )

# 대륙별 GDP per capita를 연도별로 계산하고, 그것을 선 그래프로 표현하시오. 

gapminder |> 
  summarize(
    cont_gdp = sum(gdpPercap * pop) / sum(pop),
    .by = c(year, continent)
  ) |> 
  ggplot(aes(x = year, y = cont_gdp, color = continent)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "GPD per capita", color = "Contients")
  

# 연도별로 각 국가의 인구수가 소속되어 있는 대륙의 전체에서 차지하는 비중을 구하고, 그 비중을 연도별로 순위를 매김

gapminder |> 
  mutate(
    pop_cont = sum(pop),
    .by = c(year, continent),
    pop_cont_per = pop * 100 / pop_cont,
  ) |>
  mutate(
    order_year_pop = row_number(desc(pop_cont_per)),
    .by = country
  ) 

# 국가별로 인구수의 대륙에서의 비중이 가장 높았던 해만 추리기

gapminder |> 
  mutate(
    pop_cont = sum(pop),
    .by = c(year, continent),
    pop_cont_per = pop * 100 / pop_cont,
  ) |> 
  slice_max(
    pop_cont_per,
    by = country) |> 
  count(year)

# mutate(), summarize(), reframe(), filter(), slice()에는 .by를, 나머지 slice_() 함수에는 by를 사용 --> so confusing
# https://dplyr.tidyverse.org/reference/dplyr_by.html

library(tidyverse)
library(tidymodels)

diamonds <- diamonds |> 
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

diamonds_fit <- linear_reg() |> 
  fit(log_price ~ log_carat, data = diamonds)

diamonds_fit

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()

ggplot(diamonds_aug, aes(x = cut, y = .resid)) + 
  geom_boxplot()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )


potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))

library(ggrepel)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(data = potential_outliers, aes(label = model)) +
  geom_point(data = potential_outliers, color = "red") +
  geom_point(
    data = potential_outliers,
    color = "red", size = 3, shape = "circle open"
  )


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 10)))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  labs(title = "Default, continuous", x = NULL, y = NULL)

library(patchwork)

p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p1 + p2

p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")
(p1 | p3) / p2



p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")

p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")

p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")

p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")

p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1, 3, 2, 4)
  ) &
  theme(legend.position = "top")

library(tidyverse)
library(nycflights13)

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime) # 모두 TRUE인 것만 추려냄.

# 하나의 변수에라도 NA가 있는 케이스 추려내기
flights |> 
  filter(
    if_any(everything(), is.na)
  )

# 모든 변수에 NA가 있는 케이스 추려내기: 당연히 zero
flights |> 
  filter(
    if_all(everything(), is.na)
  )

# 어떤 변수에서도 NA가 없는 완전 무결한 케이스 추려내기
flights |> 
  filter(
    !if_any(everything(), is.na)
  )


flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# 이들의 방식
flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# 나의 방식
flights |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .by = c(year, month, day)
  )

flights |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .by = c(year, month, day)
  )

# count() 함수와 summarize() 함수의 비교

flights |> 
  count(dest, sort = TRUE)

flights |> 
  summarize(
    n = n(),
    .by = dest
  ) |> 
  arrange(desc(n))

# dest별 총편수
flights |> 
  summarize(
    carriers = n(),
    .by = dest
  ) |> 
  arrange(desc(carriers))

# dest별 취항 항공사의 수
flights |> 
  summarize(
    carriers = n_distinct(carrier),
    .by = dest
  ) |> 
  arrange(desc(carriers))


flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

# 제들 방식
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(
    prop_cancelled = mean(is.na(dep_time)), 
    n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line() +
  geom_point(aes(size = n))

# 나의 방식: 이 경우는 좀 더 복잡한 것 같음.
flights |> 
  mutate(
    hour = sched_dep_time %/% 100
  ) |> 
  summarize(
    prop_cancelled = mean(is.na(dep_time)), 
    n = n(),
    .by = hour
  ) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line() +
  geom_point(aes(size = n))

# 쟤네들
flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

# 내 방식
flights |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(year, month, day)
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

flights |> 
  summarize( 
    distance_iqr = IQR(distance), 
    n = n(), 
    .by = c(origin, dest)
  ) |> 
  filter(distance_iqr > 0)

flights |> 
  summarize(
    max = max(dep_delay, na.rm = TRUE), 
    p95 = quantile(dep_delay, 0.95, na.rm = TRUE), 
    .by = c(year, month, day)
  )

flights |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE), 
    fifth_dep = nth(dep_time, 5, na_rm = TRUE), 
    last_dep = last(dep_time, na_rm = TRUE),
    .by = c(year, month, day)
  )

# 쟤네들
flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time)) |> 
  filter(r %in% c(1, max(r)))

# 내 방식: 이건 중복이 있음. 쟤네들 것이 더 좋음.
flights |> 
  mutate(
    r = min_rank(sched_dep_time),
    .by = c(year, month, day)
  ) |> 
  filter(
    r %in% c(1, max(r)),
    .by = c(year, month, day)
  )


flights |> 
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay)) +
  geom_freqpoly(binwidth = 5)

flights |> 
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) +
  geom_freqpoly(binwidth = 5, alpha = 1/5)

x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x
str_view(x)


df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))
debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )
debug

library(babynames)
glimpse(babynames)

# 벡터를 끄집어 내기: pull 함수
babynames |> 
  count(length = str_length(name)) |> 
  pull(n) |> 
  sum()

babynames |> 
  count(length = str_length(name)) |> 
  summarize(
    total_case = sum(n)
  )

babynames |> 
  count(length = str_length(name), wt = n) |> 
  summarize(
    total_case = sum(n)
  )

View(babynames)
glimpse(babynames)
sum(babynames$n)

# 연도별로 애기 이름에 x가 들어가는 것들의 비중: 이것은 이름의 빈도를 고려하지 않고, 전체 이름 중 x가 들어가는 이름의 비중임. 
babynames |> 
  summarize(
    prop_x = mean(str_detect(name, "x")),
    .by = year
    ) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

# 이것이 맞는 것으로 보임. 이름의 총 가지수가 아니라 이름의 수을 계산하여, 모든 이름에 대해 x가 들어간 이름의 비중을 계산해야함.
babynames |> 
  summarize(
    sum_freq = sum(n), # 연도별 이름의 총수 --> 출생아수 전체(빈도 5 이상의 이름을 가진 출생아수)
    sum_prop = sum(prop), # prop가 무엇인지 잘 모르겠음. 합하면 1이 되어야 하는데 그렇지 않음.
    sum_freq_x = sum(str_detect(name, "x")), # x가 들어간 이름의 총 가지 수
    sum_freq_x_2 = sum(n[str_detect(name, "x")]), # 킬링 파트: x가 들어간 이름의 총수. [] 속은 logical이므로 그것을 이용하여 n을 솎아 내고 합계를 구할 수 있음. 
    true_prop = sum_freq_x_2 / sum_freq, # 이것이 진정한 비율임
    n_name = n(), # 연도별 이름의 총 가지 수
    .by = year
  ) |> 
  ggplot(aes(x = year, y = true_prop)) + 
  geom_line()

# 위와 동일
babynames |> 
  group_by(year) |> 
  mutate(
    name_all = sum(n)
  ) |> 
  filter(str_detect(name, "x")) |> 
  summarize(
    true_prop = sum(n) / first(name_all)
  ) |> 
  ggplot(aes(x = year, y = true_prop)) + 
  geom_line() 

length(unique((babynames$year)))

# 위와 그래프의 모양이 거의 같음. 단지 Y-축의 값이 큼(배정도)
babynames |> 
  group_by(year) |> 
  filter(str_detect(name, "x")) |> 
  summarize(
    prop_x = sum(prop),
  ) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line() 


unique(babynames$year)

x1 <- c("Dec", "Apr", "Jan", "Mar")
class(x1)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
class(y1)

babynames |> 
  count(name)

x <- "\\\\\\"
str_view(x)

x <- "\U0001f604"
str_view(x)

dot <- "\\."
str_view(dot)


str_view(fruit, "(..)\\1")
str_view(words, "^(..).*\\1$")

gss_cat

relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) + 
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()


gss_cat |>
  mutate(marital = marital |> fct_infreq()) |> 
  ggplot(aes(x = marital)) +
  geom_bar()


k <- ordered(c("a", "b", "c"))
class(k)
k

a <- today()
class(a)
b <- now() 
class(b)

class
(3+2i)

2**4

a <- ymd("2017-01-31", tz = "UTC")
class(a)

a <- flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure_dt = make_date(year, month, day),
         departure_dttm = make_datetime(year, month, day, hour, minute)
  )
class(a$departure_dt)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

flights_dt |> 
  mutate(minute = minute(dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(x = week, y = n)) +
  geom_line() + 
  geom_point()

as.duration(today() - ymd("1966-11-27"))
today() - dyears(1)

as_tibble(OlsonNames()) |> 
  filter(str_detect(value, "Seoul"))

flights |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 86400)

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks |>
  complete(year = 2019:2021, qtr)

library(nycflights13)

flights <- flights

flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

diamonds

count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n))
}

diamonds |> 
  count_prop(clarity)

salsa_count_prop <- function(df, var, sort = TRUE){
  df |> 
    count({{ var }}, sort = sort) |> 
    mutate(prop = n / sum(n))
}

diamonds |> 
  salsa_count_prop(clarity)

salsa_count_missing <- function(df, group_vars, x_var){
  df |> 
    group_by(
      pick({{ group_vars }})
    ) |> 
    summarize(
      n_missing = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

flights |> 
  salsa_count_missing(c(year, month, day), dep_time)

diamonds |> 
  count(clarity, color, cut)


hex_plot <- function(df, x, y, z, bins = 20, fun = "mean") {
  df |> 
    ggplot(aes(x = {{ x }}, y = {{ y }}, z = {{ z }})) + 
    stat_summary_hex(
      aes(color = after_scale(fill)), # make border same color as fill
      bins = bins, 
      fun = fun,
    )
}

diamonds |> hex_plot(carat, price, depth, bins = 20)

diamonds |> 
  ggplot(aes(x = carat, y = price, z = depth)) + 
  stat_summary_hex(
    aes(color = after_scale(fill)), # make border same color as fill
    bins = 20, 
    fun = "mean")

sorted_bars <- function(df, var) {
  df |> 
    mutate({{ var }} := fct_rev(fct_infreq({{ var }})))  |>
    ggplot(aes(y = {{ var }})) +
    geom_bar()
}

diamonds |> sorted_bars(clarity)

rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  sample(c(rnorm(n - n_na, mean = mean, sd = sd), rep(NA, n_na)))
}

df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)
df_miss |> 
  summarize(
    across(a:d, function(x) median(x, na.rm = TRUE)),
    n = n()
  )

df_miss |> 
  across(a:d, function(x) median(x, na.rm = TRUE))

summarize_means <- function(df, summary_vars = where(is.numeric)) {
  df |> 
    summarize(
      across({{ summary_vars }}, \(x) mean(x, na.rm = TRUE)),
      n = n(),
      .groups = "drop"
    )
}
diamonds |> 
  group_by(cut) |> 
  summarize_means()



a <- c(58, 26, 24)
b <- c("이상일", "김세창", "김우형")
c <- c(TRUE, TRUE, FALSE)

df <- data.frame(a, b, c)
mean(df$a)

iris_tbl <- iris |> 
  as_tibble() 
iris_tbl

iris



students <- read_csv("https://pos.it/r4ds-students-csv")
students
students <- read_csv("https://pos.it/r4ds-students-csv", 
                     skip = 1, 
                     col_names = c("student_id", "full_name", "favorite_food", "meal_plan", "age"),
                     col_types = cols(
                       meal_plan = col_factor(),
                       age = col_integer()),
                     na = c("N/A", "")
                     )
students

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)
treatment

library(readxl)
read_excel("https://docs.google.com/spreadsheets/d/1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w/")

           
readxl_example()

deaths_path <- readxl_example("deaths.xlsx")

read_excel("https://population.un.org/")

read_excel("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")

library(openxlsx)
read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")



a <- read_excel(
  "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", 
  sheet = "Estimates"
  )

new_names <- c("index", "variant", "region_name", "notes", "location_code", 
  "ISO3", "ISO2", "SDMX", "type", "parent_code", "year", "pop_jan", 
  "pop_jul", "male_jul", "female_jul", "pop_den", "sex_ratio", 
  "median_age", "natural_change", "RNC", "pop_change", "PGR", 
  "dubling_time", "births", "birth_by_f1519", "CBR", "TFR", "NRR", 
  "mean_age_childbearing", "sex_ratio_birth", "deaths", 
  "deaths_male", "deaths_female", "CDR", "life_exp", 
  "life_exp_male", "life_exp_female", "life_exp_15", 
  "life_exp_15_male", "life_exp_15_female", "life_exp_65", 
  "life_exp_65_male", "life_exp_65_female", "life_exp_80", 
  "life_exp_80_male", "life_exp_80_female", "infant_deaths", 
  "IMR", "live_births", "under_five_deaths", "mortalty_under_five", 
  "mort_bf_40", "mort_bf_40_male", "mort_bf_40_female", "mort_bf_60", 
  "mort_bf_60_male", "mort_bf_60_female", "mort_bt_1550", 
  "mort_bt_1550_male", "mort_bt_1550_female", "mort_bt_1560", 
  "mort_bt_1560_male", "mort_bt_1560_female", "net_migrants", "NMR")

WPP_2022_estimates <- read_excel(
  "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Estimates",
  skip = 17, # skip을 적용하는 순간 첫번째 행이 공란인 컬럼의 모든 값이 날아감 --> 희안함. 그래서 col_types를 적용해야 함.
  col_names = new_names,
  col_types = c(rep("guess", 3), "text", "guess", rep("text", 2), rep("guess", 58)),
  na = c("...", "")
  )

WPP_2022_future <- read_excel(
  "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Medium variant",
  skip = 17, 
  col_names = new_names,
  col_types = c(rep("guess", 3), "text", "guess", rep("text", 2), rep("guess", 58)),
  na = c("...", "")
)

WPP_2022 <- bind_rows(WPP_2022_estimates, WPP_2022_future)

who2 |> 
  distinct(year) |> 
  max()

library(gapminder)
gapminder

library(nycflights13)
flights

library(tidyverse)
library(writexl)
write_xlsx(wpp_2022, "wpp_2022.xlsx")
read_excel("wpp_2022.xlsx")

View(gapminder)

gapminder |> 
  add_count(year, continent, wt = pop) |>
  mutate(pop_prop = pop * 100 / n) |> 
  slice_max(pop_prop, by = c(year, continent)) |> 
  filter(year == 2007)

gapminder |> 
  group_by(year, continent) |> 
  mutate(
    pop_cont = sum(pop),
    pop_prop = pop * 100/ pop_cont
  ) |> 
  slice_max(pop_prop)

# 연도별 대륙별 일인당 GDP

gapminder |> 
  group_by(year, continent) |> 
  summarize(
    sum_pop = sum(pop),
    sum_gdp = sum(pop * gdpPercap),
    gdp_percap_cont = sum_gdp / sum_pop
  ) |> 
  filter(year %in% c(1957, 2007))

gapminder |> 
  group_by(year, continent) |> 
  summarize(
    sum_pop = sum(pop),
    sum_gdp = sum(pop * gdpPercap),
    gdp_percap_cont = sum_gdp / sum_pop
  ) |> 
  filter(year %in% c(1957, 2007)) |> 
  arrange(year, desc(gdp_percap_cont)) |> 
  ggplot() +
  geom_col(aes(x = gdp_percap_cont, y = fct_reorder(continent, gdp_percap_cont))) +
  facet_wrap(~ year, scales = "free")


gapminder |> 
  group_by(year, continent) |> 
  summarize(
    sum_pop = sum(pop),
    sum_gdp = sum(pop * gdpPercap),
    gdp_percap_cont = sum_gdp / sum_pop
  ) |> 
  ggplot() +
  geom_line(aes(x = year, y = gdp_percap_cont, color = continent), size = 1)

# 연도별 대륙별 인구 비중

library(tidyverse)
library(gapminder)

gapminder |> 
  group_by(year, continent) |> 
  summarize(
    sum_cont = sum(pop)
  ) |> 
  mutate(
    sum_year = sum(sum_cont),
    prop_cont = sum_cont * 100 / sum_year,
  ) |> 
  ggplot() +
  geom_line(aes(x = year, y = prop_cont, color = continent), linewidth = 1) +
  labs(x = "Year", y = "% of Population", color = "Continents")

gapminder |> 
  count(year, continent, wt = pop) |> 
  mutate(
    sum_year = sum(n),
    prop_cont = n * 100 / sum_year,
    .by = year
  ) |> 
  ggplot() +
  geom_line(aes(x = year, y = prop_cont, color = continent), linewidth = 1) +
  labs(x = "Year", y = "% of Population", color = "Continents")

gapminder |> 
  count(year, continent, wt = pop) |> 
  group_by(year) |> # 킬링 파트
  add_tally(wt = n, name = "sum_year") |> 
  mutate(
    prop_cont = n * 100 / sum_year
  ) |> 
  ggplot() +
  geom_line(aes(x = year, y = prop_cont, color = continent), linewidth = 1) +
  labs(x = "Year", y = "% of Population", color = "Continents")


# 연도별 국가별 대륙내 인구 비중

gapminder |> 
  group_by(year, continent) |> 
  mutate(
    sum_cont = sum(pop),
    country_prop = pop * 100 / sum_cont
  ) |> 
  slice_max(country_prop) |> 
  ggplot() +
  geom_line(aes(x = year, y = country_prop, color = continent), linewidth = 1) +
  labs(x = "Year", y = "% of Population", color = "Continents")

gapminder |> 
  group_by(year, continent) |> 
  mutate(
    sum_cont = sum(pop),
    country_prop = pop * 100 / sum_cont
  ) |> 
  slice_max(country_prop) |> 
  ggplot() +
  geom_line(aes(x = year, y = country_prop, color = continent), linewidth = 1) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(x = "Year", y = "% of Population", color = "Continents")




my_plot <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Larger engine sizes tend to have lower fuel economy",
    caption = "Source: https://fueleconomy.gov."
  ) +
  theme(
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

ggsave(plot = my_plot, "graph.png", width = 6)


p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p1 + p2


ggplot(mpg, aes(displ, hwy)) + 
  geom_point(colour = "red") +
  geom_text(data = mpg |> slice_sample(prop = 0.3), aes(label = class))



library(tidyverse)
library(gapminder)
glimpse(gapminder)

# 히스토그림
gapminder |> 
  filter(year == 2007) |>
  ggplot(aes(x = gdpPercap)) +
  geom_histogram()

# 히스토그램 --> 밀도 + 정규분포

a <- gapminder |> 
  summarize(
  mean = mean(gdpPercap),
  sd = sd(gdpPercap)
)

gapminder |> 
  filter(year == 2007) |>
  ggplot(aes(x = gdpPercap)) +
  geom_histogram(aes(y = after_stat(density))) + 
  stat_function(fun = dnorm, color = "red", size = 1, 
                args = list(mean = a$mean, sd = a$sd))

# arrange
gapminder |> 
  filter(year == 2007) |> 
  arrange(desc(gdpPercap))

# 범주형 + 수치형 1
gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap)) +
  geom_density(aes(color = continent, fill = continent), alpha = 0.25)

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = fct_reorder(continent, gdpPercap, median))) +
  geom_boxplot(aes(color = continent))

# 두 범주형
gapminder |> 
  filter(year == 2007) |> 
  mutate(
    gdp_class = cut_interval(gdpPercap, n =5, labels = c("very low", "low", "middle", "high", "very high"))
  ) |> 
  ggplot(aes(x = continent, y = gdp_class)) +
  geom_count()

gapminder |> 
  filter(year == 2007) |> 
  mutate(
    gdp_class = cut_interval(gdpPercap, n =5, labels = c("very low", "low", "middle", "high", "very high"))
  ) |> 
  count(continent, gdp_class) |> 
  ggplot(aes(x = continent, y = gdp_class)) +
  geom_tile(aes(fill = n))



# 두 수치형

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes()) +
  geom_smooth()

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_bin_2d()

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_boxplot(aes(group = cut_width(gdpPercap, 5000)))


gapminder |> 
  filter(year %in% c(1987, 1997, 2007)) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)


# 세 변수 이상

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = log(gdpPercap), y = log(lifeExp))) +
  geom_point(aes(size = pop)) +
  geom_smooth()

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = log(gdpPercap), y = log(lifeExp))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~continent)

library(tidymodels)


gapminder |> 
  group_by(year, continent) |> 
  mutate(
    sum_pop = sum(pop)
  ) |> 
  ggplot(aes(x = year, y = sum_pop)) +
  geom_area(aes(fill = fct_reorder2(continent, year, sum_pop))) +
  scale_x_continuous(breaks = seq(1952, 2007, 5), labels = seq(1952, 2007, 5)) +
  scale_y_continuous(breaks = seq(0, 7000000000, 1000000000), 
                     labels = format(seq(0, 7000000000, 1000000000), big.mark = ",", scientific = FALSE)) +
  labs(x = "Year", y = "Population", fill = "Continent") +
  scale_fill_brewer(palette = "Set3")


gapminder |> 
  group_by(year, continent) |> 
  mutate(
    a = fct_reorder2(continent, year, population)
  )
  
library(rea)
read_excel("https://github.com/awalker89/openxlsx/raw/master/inst/readTest.xlsx")

library(googlesheets4)
gs4_deauth()
read_sheet("https://docs.google.com/spreadsheets/d/1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY/edit#gid=780868077")


# 네이버

library(httr2)
library(httr)
library(rJava)
library(KoNLP)
library(tidyverse)


my_ClientID <- "hYwglpLz9Cq8Oyn6hzgE"
my_Client_Secret <- "m7onDeHfwe"
my_url <- "https://openapi.naver.com/v1/search/news.json"

news <- GET(url = my_url, 
            add_headers("X-Naver-Client-Id" = "hYwglpLz9Cq8Oyn6hzgE",
                        "X-Naver-Client-Secret" = "m7onDeHfwe"),
            query = list(
              query = "지방소멸",
              dispaly = 100, 
              start = 1,
              sort = "date"
            ))
content(news)

content(news)$total

all_news <- data.frame()
search <- "지방소멸"

for(i in 1:10){
  param = list(query = search,
               display = 100, 
               start = i,
               sort = "date")
  news <- GET(url = my_url,
              add_headers("X-Naver-Client-ID" = my_ClientID,
                          "X-Naver-Client-Secret" = my_Client_Secret),
              query = param)
  body <- data.frame(t(sapply(content(news)$item, data.frame)))
  
  all_news <- rbind(all_news, body)
  Sys.sleep(0.1)
}


search <- "지방소멸"

req_naver <- request(my_url)

headers_naver <- req_naver |> 
  req_headers(
    "X-Naver-Client-ID" = my_ClientID,
    "X-Naver-Client-Secret" = my_Client_Secret
  )

query_naver <- headers_naver |> 
  req_url_query(
    query = search,
    display = 100,
    start = 1,
    sort = "date"
  )

resp_naver <- query_naver |> 
  req_perform()

result_naver <- resp_naver |> 
  resp_body_json()

result_naver$items

my_ClientID <- "hYwglpLz9Cq8Oyn6hzgE"
my_Client_Secret <- "m7onDeHfwe"
my_url <- "https://openapi.naver.com/v1/search/news.json"

result_naver <- request(my_url) |> 
  req_headers(
    "X-Naver-Client-ID" = my_ClientID,
    "X-Naver-Client-Secret" = my_Client_Secret
  ) |> 
  req_url_query(
    query = search,
    display = 100,
    start = 1,
    sort = "date"
  ) |> 
  req_perform() |> 
  resp_body_json() 

naver_data <- data.frame(t(sapply(result_naver$items, data.frame)))


all_news <- data.frame()
search <- "지방소멸"
for(i in 1:10){
  news <- request(my_url) |> 
    req_headers(
      "X-Naver-Client-ID" = my_ClientID,
      "X-Naver-Client-Secret" = my_Client_Secret
    ) |> 
    req_url_query(
      query = search,
      display = 100,
      start = i,
      sort = "date"
    ) |> 
    req_perform() |> 
    resp_body_json()
  body <- data.frame(t(sapply(news$item, data.frame)))
  all_news <- bind_rows(all_news, body)
  Sys.sleep(0.1)
}

