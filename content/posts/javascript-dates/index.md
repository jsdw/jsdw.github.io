+++
title = "Notes on Working with Dates in JavaScript"
description = "A few things that are worth knowing if you intend to work with Dates in JavaScript"
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

And here is another example, this time using the `dateFns` library:

```
> dateFns.eachDay('2019-07-05T00:00+00:00', '2019-07-07').map(d => d.toISOString())
["2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
> dateFns.eachDay('2019-07-05T00:00+01:00', '2019-07-07').map(d => d.toISOString())
["2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
> dateFns.eachDay('2019-07-05T00:00+02:00', '2019-07-07').map(d => d.toISOString())
["2019-07-03T23:00:00.000Z", "2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
```

Let's find out what just happened.

FIrst off, here are some important things to know about Date handling in JavaScript (and in general):

**Important thing to know #1**: A _timezone_ like "Europe/London" encodes the idea that there is some offset from UTC+0 that must be applied to get to local time in this area. Additionally, it can encode changes to that offset over time, for example due to daylight savings time.

**Important thing to know #2**: My timezone is "Europe/London". This timezone encodes the fact that from 31st March 2019 until 27th October 2019 my time is offset from UTC by +1 hour (British Summer Time), and the rest of the year it's equal to UTC time (Greenwich Mean Time). This will explain some of the results I get parsing dates below.

**Important thing to know #3**: `Date`s are just unix time stamps under the hood (milliseconds from 1st January 1970 in UTC). A `Date` on its own knows nothing about timezone or offset.

**Important thing to know #4**: To convert a string into a `Date` (ie into a unix timestamp), the offset from UTC of the string needs to be known. This might be explicitly provided, or set implicitly based on some rules. If we know the offset from UTC, we know how to convert the parsed date to a unix timestamp.

Armed with these tidbits, let's address the first example and have a look at how some strings are parsed into `Date`s. In each example, I show the UTC+0 ISO date string that we end up with (which is equivalent to a unix timestamp but easier to read for us humans):

- `new Date("2019/07/05").toISOString() == "2019-07-04T23:00:00.000Z"`: Assumed to be midnight 5th July BST (the offset for this date according to my timezone). Thus, we knock an hour off to get our UTC time.
- `new Date("2019-07-05").toISOString() == "2019-07-05T00:00:00.000Z"`: Oddly, this is assumed to be midnight 5th July UTC, ignoring my local timezone. It is therefore 1 hour later than the above.
- `new Date("2019-07-05T00:00").toISOString() == "2019-07-04T23:00:00.000Z"`: Adding a `T` with valid time but no explicit offset makes this parse as local time again.
- `new Date("2019-07-05T00:00+00:00").toISOString() == "2019-07-05T00:00:00.000Z"`: As soon as we add an explicit offset (here, `+00:00`, so UTC time), it's honoured.
- `new Date(Date.UTC(2019,6,5)).toISOString() == "2019-07-05T00:00:00.000Z"`: `Date.UTC` returns a unix timestamp (which is always UTC+0). Note that the month argument of `Date.UTC` is zero indexed for fun.

As soon as the string has been parsed into a Date, all timezone/offset information is lost and it's just a unix timestamp. Functions on the `Date` object then let you view timestamp in terms of your current local timezone. For example, `date.getHours()`, `date.getMinutes()` and so on. UTC equivalents of these exist if you'd rather view the timestamp in terms of the current UTC+0 day/month/year/etc that it resolves to (eg `date.getUTCHours()`, `date.getUTCMinutes()`).

A couple of useful timezone related functions:
- `Intl.DateTimeFormat().resolvedOptions().timeZone`. This returns the current system timezone on browsers that support it ("Europe/London" for me).
- `date.getTimezoneOffset()`. Used on some date, this returns the offset from UTC in minutes with respect to your system timezone.
  - `new Date('2019/07/05').getTimezoneOffset() == -60` for me, since it is BST in my timezone at that time.
  - `new Date('2019/01/05').getTimezoneOffset() == 0` as my timezone is in GMT (UTC+0) at that time.

If you wish to view dates with respect to some other timezone (perhaps to allow users to set their timezone in your application and ignore their system timezone), you'll need:
- A library that knows how to work with different timezones (eg `moment-timezone`).
- A database of timezone information, which contains information as to what offsets happen when for each timezone (this is often provided with the library).

If you work with date libraries, be aware that they may be manipulating dates based on their time in your local timezone (eg by using `date.setDays()`, `date.setHours()`) or in UTC time. With this in mind, let's break down the second example and see what's going on:

```
> dateFns.eachDay('2019-07-05T00:00+00:00', '2019-07-07').map(d => d.toISOString())
["2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
```

In this first case, my start date is UTC+0. I get back each day, but zeroed out with respect to local time. In order to do this, `dateFns` must be applying `-getTimezoneOffset()` minutes to the first input date to transform it from UTC to the local timezone. It can then use that as a starting point to produce each day in local time.

```
> dateFns.eachDay('2019-07-05T00:00+01:00', '2019-07-07').map(d => d.toISOString())
["2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
```

In this second case, we provide dates that are equal to local time for my timezone (in other words, the start date is the same as `2019-07-05T00:00` or `2019/07/05`). Simply iterating from the start day (in local time) will work fine here.

```
> dateFns.eachDay('2019-07-05T00:00+02:00', '2019-07-07').map(d => d.toISOString())
["2019-07-03T23:00:00.000Z", "2019-07-04T23:00:00.000Z", "2019-07-05T23:00:00.000Z", "2019-07-06T23:00:00.000Z"]
```

In this third case, I've picked an offset that is greater than my local time offset. Thus, even when adding `-getTimezoneOffset()` minutes onto the date given, it's still a day behind in local time. So, we end up starting from a day earlier when producing the range.

I think that the key takeaway from this post should be that dates in JavaScript are just unix timestamps. In order to work with the days/months/years etc, we have to view that timestamp with respect to some timezone. natively, we can view it in terms of local time or UTC time, but libraries allow us to view it with respect to other timezones as well. Finally, be aware of the assumptions that libraries make when you pass them dates, to avoid surprise.
