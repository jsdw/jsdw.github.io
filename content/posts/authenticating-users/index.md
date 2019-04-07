+++
title = "Authenticating users on your site"
description = "You're building a website that you want users to be able to log in to. Whether you use an external service or roll your own, here are a few things to keep in mind."
date = 2019-04-07
[extra]
created = "2019-04-07"
+++

So, you want users to be able to log in to your site. Here are some things that are worth considering.

# Use HTTPS

However you choose to authenticate users, use HTTPS. If you do not, somebody can sit between a user and your application and see the password that they enter being sent across plain text, or (if you avoid doing it yourself) see the access code handed to your app. If they do this, they have the chance to masquerade as that user or worse.

HTTPS encrypts communication between the users browser and your application, making this impossible. Somebody snooping on the network traffic can see which IP addresses your browser is communicating with, but they can't see the actual URL or data being transferred back and forth.

# Can you avoid doing it yourself?

Github, Google, Facebook and various other services allow users to authenticate with *them* instead. **OAuth** is a common protocol for doing authorization (and authentication as a by product), and a common OAuth flow works like so:

1. A user that wants to log in is redirected to the authentication service.
2. The user logs in with them instead, and then they are redirected back to your app with an *access code*.
3. Your app then talks directly to the service, exchanging that access code with an *access token*.
4. Now, your app can use the access token to obtain information about the user from the service.

Now, your app can use the access token to obtain some unique ID for the user, and associate state (for example, a shopping cart) with that ID. It doesn't ever have to store passwords, handle password resets, or handle registering users. A bunch of these services also offer multi-factor auth.

One downside is that users have to register with one of the services you use instead (the upside is that they probably already have done so).

Something to be aware of is that you still have to treat the access token you obtain securely, and you still have to think about *sessions* (see below).

# Don't store passwords

If you want or need to implement your own authentication system, don't store passwords in your database. Hash passwords using a *cryptographic hash function* designed for this purpose, such as `bcrypt`. This converts a password into a fixed-length random-looking sequence of bytes. You can then store that instead. Things to consider when hashing passwords:

- Cryptographic hash functions are one-way, so a hacker cannot convert the bytes back into a password. They *can* however try hashing lots and lots of passwords until they find one that hashes to the *same* string of bytes. This means that the hash function should be **slow**, so that it takes a substantial amount of time to do so. `bcrypt` is deliberately slow for this reason.
- You can configure how slow hash functions like `bcrypt` are; this is the *cost factor*. The lower it is, the quicker somebody can brute force a hash to work out what password was used to create it. Pick a speed that is fast enough for your needs but no faster.
- Hackers can build up a map from hash to password for common hash functions like `md5`. To defeat this, always *salt* your passwords. A salt is some random bytes, ideally generated using a *Cryptographically Secure Pseudo-Random Number Generator* (CSPRNG). Append these bytes to the password before hashing it, and also store those bytes in your database next to this hashed password. Now, we can append the same salt to password attempts before hashing them to compare with the original.
- Use a *different* salt for every password you hash. This means that even if multiple users use the same password to log in to your app, they will all look completely different when hashed.
- Instead of manually working with salts, just use `bcrypt`, which does it all for you automatically. `bcrypt` generates a salt and appends it to the output hash so that it can compare new password attempts against the hashed one.
- It might be worth adding a *pepper* to passwords as well. Do to this, store some random string of bytes outside of your database, and encrypt each hashed password with it. if a hacker compromises your database, the encrypted passwords will be utterly useless if they do not also find the pepper so that they can decrypt them.

This boils down to just using `bcrypt` or something similar with a decent cost factor, and possibly encrypting passwords with a pepper for that extra layer of security.

# Don't have an unhelpful password policy

Longer passwords are normally better, so don't arbitrarily limit your user to having a really short password.

There are technical limits to password size, but there shouldn't be anything wrong with a password that is a couple of hundred characters in length.

[This Page][troy-hunt-auth] covers a bunch of useful bits and is well worth a read.

# Sessions and JWTs

Browsers do not maintain any sort of persistent connection with your application. Once a user has logged in, their browser will need to let your app know who they are each time it communicates with it.

This is what *sessions* are for. The basic approach is this:

1) When a user logs in, your application generates a random session ID, and stores it in a database alongside the ID of the user that's just logged in.
2) Your application then sends this session ID back to the user's browser (normally in the form of a *Set-Cookie* header).
3) Each time the user's browser communicates with your app, it sends this session ID to it (this happens automatically if it was set as a Cookie). The application can then look it up and find the corresponding user ID.

Session IDs should be random and sufficiently long; somebody should not be able to work out what any other session IDs are from the one that they have. If they manage to do this, they can pretend to be somebody else.

As an alternative to generating a random ID, you might decide to create a *JSON Web Token* (JWT) instead. This is a token constructed in a standard format that can contain arbitrary data, and can be *signed* and optionally *encrypted*. If the signature is valid, it guarantees that the JWT has not been modified, and was created by your app. If it's encrypted, the browser can't view the content of the JWT.

An advantage of JWTs is that they if they contain enough information, your application can avoid a database lookup, since it can verify and trust the contents of the token. This can help when building distributed and stateless apps. A disadvantage is that, since the token is trusted without any need for database checks and so on, you cannot *untrust* them. A valid token will continue to be valid until it expires. Some notes about JWTs:

- JWTs define *how* they are signed in their header. Make sure that you expect them to be signed in a certain way, and don't allow tokens that have not been signed in this way (an unsigned token is perfectly valid, but no longer has any guarantees about who made it).
- JWTs are **not** encrypted by default, so while they look opaque because they are base64 encoded, users can read the contents of them.
- If you hand the JWT to a browser, make sure it doesn't live for too long. If it gets stolen, the malicious user will have free reign until it expires. One way to limit the chance of it getting stolen in the browser is to place it in an HTTP-only cookie, so that JavaScript can't read it.
- If you do urgently need to invalidate tokens, and you control enough of the infrastructure, you can change the public/private key pair that was used to sign them, so that existing tokens will no longer have a valid signature. However, the public key has probably been cached anywhere that validates the token, and so you'd need to be able to clear those, too.

For simple apps, I think it's easier to generate and use your own session IDs. The database hit is often acceptable, they can be revoked if needed by simply deleting the session from the database, and they are pretty simple to understand and therefore do right. You may have a good use case for JWTs though.

[troy-hunt-auth]: https://www.troyhunt.com/passwords-evolved-authentication-guidance-for-the-modern-era/
