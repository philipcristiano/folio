use chrono::{DateTime, Datelike, NaiveTime, Utc};
use chronoutil::relative_duration::RelativeDuration;
use chronoutil::rule::DateRule;

pub fn month_ranges(
    from: DateTime<Utc>,
    num_back: i32,
) -> anyhow::Result<Vec<(DateTime<Utc>, DateTime<Utc>)>> {
    let naive_now = from
        .naive_utc()
        .date()
        .with_day(1)
        .ok_or_else(|| anyhow::anyhow!("Error"))?;
    let start = naive_now + RelativeDuration::months(1);
    let delta = RelativeDuration::months(-1);
    let size: usize = num_back.try_into()?;
    let rule = DateRule::new(start, delta).with_count(size);
    let time = NaiveTime::from_hms_opt(0, 0, 0).ok_or_else(|| anyhow::anyhow!("Error"))?;
    Ok(rule
        .map(|d| {
            (
                DateTime::from_naive_utc_and_offset((d + delta).and_time(time), Utc),
                DateTime::from_naive_utc_and_offset(d.and_time(time), Utc),
            )
        })
        .collect())
}
