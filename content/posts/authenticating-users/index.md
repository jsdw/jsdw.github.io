+++
title = "Rolling your own user authentication (what could possibly go wrong?)"
description = "You're building a website that you want users to be able to login to, and you don't want to shell out for off the shelf solutions like Auth0. Let's try and roll our own."
date = 2019-03-31
[extra]
created = "2019-03-31"
+++

So, you have a website that needs to be able to identify who is currently using it. In order to do this, the site needs to know who a given person that's using it is. It needs to be able to *authenticate* them.

A common way to authenticate somebody is to make them sign up to your site by going through some registration process. Once they've done this, they can prove that they are that same person each time they visit your site by *logging in*, and providing the same unique ID (an email address for instance) and password that they provided on registering.

                                    [Registration process diagram]

Instead of making them prove who they are to you, some services (Facebook, Google, Github and many others) let users prove who they are to *them* instead. A common protocol for this is called OAuth. In one form of this, users that wish to authenticate are redirected to the external service and made to log in there. The external service them redirects them back to your site with an *access code*, which it can then exchange for an *access
token* behind the scenes. Your site can then use the access token to access information about the user that's just logged in from the external service, including a unique ID.

                                    [OAuth Diagram here]

Now that the user has logged in (and the site has a unique ID representing them), each time their browser communicates with your site (for example to refresh the page or load new content), your site needs to be reminded who they are. In other words, a *session* needs to be established. A common way to do this is for your site to associate the unique ID it now has with some unique token, which it gives back to the user's browser when they authenticate. The browser can then pass this same token back to the server each time it talks to it, and from this the site can look up the unique ID associated with it.

In many cases, using an external service that you trust to do the hard work of authenticating a user is the best option. You don't have to store as much (or indeed any) potentially sensitive user information, build an interface to allow users to sign up, handle forgotten passwords, or worry about the potential security holes you could end up with if you roll your own.

That said, sometimes it's not a bad idea to roll your own solution. For one thing, using an external service does not absolve you of the need to think carefully about the various security implications around it. You'll still end up with something like an access token that you need to keep safe, for instance. Perhaps you simply don't want to rely on users having to have signed up with any external service in order that they can then log in to that service in order to authenticate with your site.

# Rolling your own

Let's start with a scenario with some issues, and then walk through how to do it better:

*First thing's first; users will need to be able to register on your site. So you figure, let's keep it simple and assume that to sign up, they are going to provide just a username and a password. The first thing you'll need is some place to put this information, so you make yourself a database (or use a hosted one) to put it all in. Great! Now, when a user wants to log in, you reason, they will simply enter the same username and password that they used when they signed up. Those get sent off to your site's server, which looks them up in the database and confirms that they match or not.*

*If the username and password match, you need to somehow mark them as logged in, so that when they refresh the page or navigate around it, we know who they are. This is what cookies are for! So you have your server set a cookie on the user's browser called "loggedIn" that contains some string, which we associate in our database with the user that's just logged in. Every time they refresh the page, their browser sends your server that cookie, and the site can look up what user it's associated with to know who they are. Great, job done!*

Now, let's look at each part of this and fic any problems we find with it.

## Registration

*"to sign up, they are going to provide just a username and a password"*

The first thing that this implies is that the user will have to send details, including a password, to your server. This means that you want the connection between the user's browser and your server to be encrypted, so that people can't watch the traffic going back and forth and see the user's password and such.

To do this, you need to make sure your site is being served over **HTTPS**. This does exactly that. It's also important to make sure that any assets (scripts, images, and so on) that your site requests are also served over HTTPS. If they aren't, an attacker could intercept and modify that data to do scary things like injecting custom code into your site which steals the user's details.

When it comes to looking at the kinds of passwords that you'll allow users to pick, there are a bunch of things to consider. [Read this excellent post][troy-hunt-auth] for a bunch of useful guidance in that area.

Finally, a username is fine, but what happens if the user forgets their password? It might make more sense to require that users provide an email address instead, which you can verify on registration and send password reset requests to if necessary.

## Storing passwords

*"so you make yourself a database (or use a hosted one) to put it all in"*

This is a bit vague, but somewhat implies that we'll be storing their password in a database, which is a big no-no. Usefully, there is a class of functions—cryptographic hash functions—that perform a one way conversion from some arbitrary data (such as a password) into a chunk of data that is a small, fixed size. This is known as *hashing*. Using one of these functions, you could hash the password that the user provides at registration time and then store the result of doing so instead. Now, to check the password that they enter at login time, just hash that too and check to see whether it matches what you stored earlier.

This is far from the end of the story though! For one thing, many hash functions (like `md5`) are designed to be really fast, which is great if you are trying to compare two big chunks of data with each other, but less good if

but this means that if somebody gets hold of the hashed password, they can crack it by hashing billions of passwords per second in a brute force approach until they find a password that matches.

Worse, people have built up [rainbow tables][rainbow-table], which are just large tables mapping from hash to original password. Instead of doing all of the work yourself, you can crack a hashed password by looking up the hash in one of these.

To combat this, you want to use a hash function designed for hashing passwordsd like `bcrypt`. `bcrypt` is deliberately slow at hashing passwords, which means that brute forcing a bcrypted hash takes far too long. It also adds a *salt*—a random set of characters—to the password prior to hashing it (and then displays the salt in the output). This makes it immune to rainbow tables, since if you hash a password with two different salts, it will look completely different.

If your passwords are hashed with something like `bcrypt`, they are already pretty safe now, however if they are stolen, it is possible that an attacker could eventually find a password that mapped to that same hash (if the user picked a poor one, at least). Some people therefore recommend also adding a *pepper* to passwords prior to hashing. This is a bunch of random characters that is stored outside of your database. Added to even a weak password, it will make it practically impossible to crack, as long as attackers don't gain access to it.

                                    [Salt and Pepper]

Rather than appending a pepper to the password prior to hashing, you might prefer to encrypt your password using the pepper as the key. This way, you can decrypt and re-encrypt passwords when you want to change the pepper, and once again the resulting output (hashed with a salt and then encrypted with a pepper) is useless to anybody unless they know the pepper too.

# Sessions

*"So you have your server set a cookie on the user's browser called "loggedIn" that contains the username of the user that has logged in"*

[troy-hunt-auth]: https://www.troyhunt.com/passwords-evolved-authentication-guidance-for-the-modern-era/
[rainbow-table]: https://en.wikipedia.org/wiki/Rainbow_table