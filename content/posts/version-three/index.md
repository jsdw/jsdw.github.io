+++
title = "Website Reboot: Version Three"
description = "Welcome to the third incarnation of my website, this time built to be simple, slick, and to the point."
date = 2014-01-26
+++

Since I built the last iteration of this site back in early 2013, I've come a long way (with web development inparticular) and learned a lot of cool things, so it felt like time to reflect that with a new site and a renewed dedication to putting down what I've learned in writing.

For me, this is going to be the year of AngularJS, which is frankly one of the coolest new technologies I've had the pleasure of working with these last few months. We're building some awesome stuff with it at work, so it seemed fitting to put some of that to use on this new website.

# Memory Lane

Last time round, I wanted to create a single-page AJAX driven site, that pulled in only what was needed, and was quick and responsive. It was an interesting challenge, and led to several realizations. One of these was that it could quickly become incredibly hard to keep the website in sync with the backend.

Consider the act of logging in for instance. On a more traditional site, the act of logging in leads to a complete page refresh as the server delivers you an entirely new page. This means that as long as the server is aware of your authentication status (logged in, or not logged in), it can provide you a new page filled with all the right content. On a single-page website, the act of logging in requires your page to surgically remove all of the pieces that are no longer relevant and add in new bits that are. Throw in the need to switch content around anyway as the user navigates through different parts of your single-page app, and you can quickly end up with a complex series of checks just to make sure your page is showing you content appropriate to your authentication status. Now what happens if your users have varying permissions, which will determine what is displayed? The whole thing can quickly become a spaghettified mess of code.

The next issue comes when you need to maintain the functionality of the browser back and forward buttons. On a traditional multi-page site, that all just works. On a single page site, there are no URL changes by default, and thus no notion of moving forwards or backwards through pages for your browser to latch on to. It all has to be reimplemented somehow, and kept in sync with what the user is actually doing on your site.

On the last iteration of this website, I [implemented things myself][ajax-tools]. I made a URL routing system which reacted to changes to a part of the URL (the hash) which does not cause a page reload, so that I could do my own reloading by switching content around and so on according to what this part of the URL was. Pretty standard fare for driving single-page apps, though the newer [HTML5 history API][html5-history] will eventually supersede it when browser support grows. I also made a storage system which one could attach events to. This stored things such as the authentication state, and made sure that relevant functions fired and sliced things around when the users authentication state changed. This did the trick on my rather simple site, but the code would have no doubt gotten out of hand on a more complex one.

# Enter Angular

Still, new site, new rules. This time, I was going to do it right. Enter [AngularJS][angular].

Angular is a framework made by some guys at Google which is designed specifically for making your life a damn sight easier when developing single page apps. Specifically, it takes the pain out of keeping your HTML in sync with your _model_ of how things should be. In essence, your model of the page is just a bunch of Javascript data; variables, objects, arrays, whatever works. Angular provides a means to tie these to your HTML. You can say that you want bits of HTML to display when some variable is set to true, or populate elements with content stored in variables, for instance. It also goes the other way, and keeps variables in sync with changes in the HTML. For example, you can tie the content of a _textarea_ with the content of some variable, and the two will forever stay in sync with eachother.

Needless to say, it makes complex single-page apps possible with far less headache, and I sort of love it at the moment, so you can expect some Angular related posts in the near future. This website is built with Angular at its core, which makes it way easier for me to maintain and update as time goes on.

One of the other motivations for redoing the site (other than the fact that I get bored of things quite quickly) is to have a bit of a cleanup. Last time round, I wanted to do everything by myself. One of these things was the comment system. I dare say it was quite nice, albeit sparse on features, but it led to a lot of pain. Having a comment system required the notion of multiple users. Multiple users required the facility for users to register with the site, which involved email sending and confirmation codes, as well as sensitive details (passwords for instance) being wired across the internet. This meant that I had to use HTTPS to properly secure the communication between client and site, and had to implement session handling to keep track of users so that the page would know when someone was logged in, and who they were. This meant I had to use signed cookies (I was going to do it right), Bcrypt hashing, and more tables in my database. I also wanted to have nestable comments, and Gravatar icons for users, so that it was functional and didn't look naff. And that's only the stuff I can remember off the top of my head.

All so that people could comment on my site.

This was made use of maybe twice (partly because it was too many hoops to make people jump through), but the amount of extra work it required probably formed a substantial amount of the total time spent coding up the backend. I learned some cool things, so I don't overly regret it, but this time round I wanted to keep things simple, so I'll be integrating a third party commenting system into the site instead. This will make both my life and yours (if you are of the commenting nature) much simpler. So watch this space (or below it).

# Slimming Down

Not needing comments, and using a much better build process than before (there wasn't one before) actually meant that I could serve an entirely static page, eliminating the need for any fancy server stuff. Now, posts just take the form of JSON files containing their metadata (title, keywords, date, location of contents), and the front-end knows hoe to get hold of this, so no database cleverness is required. It also means that I could organize my content better, rather than having bits living in a database, and other bits in similarly named folders somewhere else. Not to mention that it should be even faster than it was, and my server could support a ridiculous number of pageviews (a man can dream).

What's left is the fat free version, but with all of the taste.

Anyway, it's still a work in progress, so I'll be tweaking things here and there and adding bits in when I get the time, but everything should (hopefully) work just fine! The keyword system is an improved version on before, so you can filter my posts based on keywords (for example filtering by "releases" will show everything I have released thus far), and the simple layout means it's also super responsive to screen size changes, adapting for smaller displays with ease. All in all, I'm pleased, and I look forward to getting more content up and smoothing over the rough bits!

Stay tuned for Angular and Golang posts in the pipeline, and let me know what you think to the new layout (once I've added comments, of course!).


[ajax-tools]: ./posts/ajax-tools/index.md
[html5-history]: http://developer.mozilla.org/en-US/docs/Web/Guide/API/DOM/Manipulating_the_browser_history
[angular]: http://angularjs.org