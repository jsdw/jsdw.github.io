+++
title = "Working with Dates in JavaScript"
description = "A few things worth knowing if you work with Dates in JavaScript"
date = 2019-05-18
[extra]
created = "2019-05-18"
+++

What's worth knowing about Dates in JavaScript? Well, this is the example that motivated this post:

```
> new Date("2019/07/05")
Fri Jul 05 2019 00:00:00 GMT+0100 (BST)
> new Date("2019-07-05")
Fri Jul 05 2019 01:00:00 GMT+0100 (BST)
```

Hopefully by the end of this short post you'll know what just happened.

**Important thing to know #1**: My timezone is "Europe/London". This timezone encodes the fact that from 31st March 2019 until 27th October 2019 my time is offset from UTC by +1 hour (British Summer Time), and the rest of the year it's equal to UTC time (Greenwich Mean Time).

**Important thing to know #2**: `Date`s are just unix time stamps under the hood (milliseconds from 1st January 1970 UTC). A `Date` on its own knows nothing about timezone or offset.

**Important thing to know #3**: When you want to convert a string into a `Date` (ie into a unix timestamp), a decision has to be made about the offset from UTC that the string is assumed to be parsed with respect to, so that we can convert it to the UTC+0 timestamp. Here are some examples of what different date strings parse to:

- `new Date("2019/07/05").toISOString() == "2019-07-04T23:00:00.000Z"`: Assumed to be midnight 5th July BST (the offset for this date according to my timezone). Thus, we knock an hour off to get our UTC based ISO 8601 string.
- `new Date("2019-07-05").toISOString() == "2019-07-05T00:00:00.000Z"`: Oddly, this is assumed to be midnight 5th July UTC, ignoring my local timezone. It is therefore 1 hour later than the above.
- `new Date("2019-07-05T00:00").toISOString() == "2019-07-04T23:00:00.000Z"`: Adding a `T` with valid time but no explicit offset makes this parse as local time again.
- `new Date("2019-07-05T00:00+00:00").toISOString() == "2019-07-05T00:00:00.000Z"`: As soon as we add an explicit offset (here, `+00:00`, so UTC time), it's honoured.
- `new Date(Date.UTC(2019,6,5)).toISOString() == "2019-07-05T00:00:00.000Z"`: `Date.UTC` returns a unix timestamp (which is always UTC+0). Note that the month argument of `Date.UTC` is zero indexed for fun.

As soon as the string has been parsed into a Date, all timezone/offset information is lost and it's just a unix timestamp. Functions on the `Date` object then let you view timestamp in terms of your current local timezone. For example, `date.getHours()`, `date.getMinutes()` and so on. UTC equivalents of these exist if you'd rather view the timestamp in terms of the current UTC+0 day/month/year/etc that it resolves to (eg `date.getUTCHours()`, `date.getUTCMinutes()`).

A timezone is strictly more useful than an offset. A timezone describes a location, and knows what the offset from UTC will be at different times. New laws may lead to the offset from UTC changing from year to year within some timezone, but the timezone itself would remain the same.

A couple of useful timezone related functions:
- `Intl.DateTimeFormat().resolvedOptions().timeZone`. This returns the current system timezone on browsers that support it ("Europe/London" for me).
- `date.getTimezoneOffset()`. Used on some date, this returns the offset from UTC in minutes with respect to your system timezone.

If you wish to view dates with respect to some other timezone (perhaps to allow users to set their timezone in your application and ignore their system timezone), you'll need:
- A library that knows how to work with different timezones (eg `moment-timezone`).
- The database of timezone information, which contains information as to what offsets happen when for each timezone (this is often provided with the library).

If you work with libraries like `dateFns`, bear in mind that they might be formating dates and so on in terms of local time rather than UTC time. `dateFns.eachDays` for instance zeroes out the time with respect to the current system timezone, which may lead to issues if you then want to interpret the output Dates as UTC.

That about covers it. Good luck with that date handling!
