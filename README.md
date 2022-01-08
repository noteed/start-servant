# Prototyping with Servant and STM

This is an example application to demonstrate how Servant and STM can be used
to prototype a classical three-tier web application (where STM replaces a
traditional relational database).

By prototype, we mean a software artifact that documents how the real software
should work, documents what the business logic is, and provides confidence in
the business logic by virtue of having a working implementation.

Since the prototype is principally meant to inform things, a page showing, say,
a draft invoice, can also show more than what a real application would. E.g.:
show the available operations, even the one the current user cannot apply, or
show a graphical representation of its state machine, if any.

The prototype should make clear (sometimes by providing prose or graphics
instead of code):

- Objects: What are the main concepts (data types) that are entered in,
  manipulated through, and stored by the system.
- Operations: What are the operations users of the system can carry. These are
  things that are availble through some GUI or API and appear to be done
  atomically, or things that are taken care of by users outside the system but
  should be reported to the system.
- Views (pages): The possible views one can have of the underlying data.
- Rights: The views and operations that depend on access rights and
  permissions.
- Steps (queues): The operations that are possible depending on the current
  state of an object.
- Valid states: The list of checks or constraints that the state of the system
  must conform to. For each, examples of valid and invalid cases should be
  provided.

So it is a working implementation, in addition of the views, the prototype
provides forms and/or API endpoints to run the available operations.

When objects are in a state where subsequent operations can be carried, they
act similarly to tasks in to-do lists, which act themselves as queues. Those
queues and the users that are allowed to (or must) carry the next operation
should be clearly visible in the prototype.

CRUD (especially the U) should be avoided and instead operations with clear
business meaning should be defined. For instance, if a received invoice has to
be paid, even if the paiement if conducted manually outside the system, the
system must provide a, say, `MarkAsPaid` operation, instead of a generic update
that happens to change the state field.

The two preceding paragraphs are interesting because they mean the prototype
can account for business operations that are not yet implemented in software,
or that could by-pass the implemented version.

A "side-effect" of having a quick-to-develop prototype with an in-memory state,
is that it can be used to write multi-steps forms (whose final states can be
submitted to a real system).

Finally, example data and test scenarios should be provided (e.g. to be able to
browse views easily). This would allow to turn the prototype as a kind of test
suite.


# Organization

This started with code form the `servant-auth` `README`, adapted to:

- Use a simple form, instead of a JSON payload, to login
- Use a 303 redirect insted of 204 upon succesful authentication
- Show some HTML in addition of JSON
- Use STM to have an in-memory store with atomic operations

See https://github.com/haskell-servant/servant-auth.

- Operations are reified as data types, instead of being just functions.
- Use the Handle pattern ?
  - https://blog.noredink.com/post/658510851000713216/haskell-for-the-elm-enthusiast
  - https://jaspervdj.be/posts/2018-03-08-handle-pattern.html
  - https://www.schoolofhaskell.com/user/meiersi/the-service-pattern
  - https://github.com/jaspervdj/fugacious


# Authentication using cookies with cURL

cURL can save (with `--cookie-jar`) and read (with `--cookie`) cookies. So with
our server running, we can save a cookie file with Servant's `JWT-Cookie`, then
use that cookie file for the next request:

```
$ curl --cookie-jar a -d username="alice" -d password="secret" http://127.0.0.1:7249/login
$ curl --cookie a -H 'accept: text/html' http://127.0.0.1:7249/settings/profile
```


# Exemple requests


The following calls use the same route, but with a different `Accept` HTTP
header:

```
$ curl --cookie a http://127.0.0.1:7249/a/settings/profile
$ curl --cookie a -H 'accept: text/html' http://127.0.0.1:7249/a/settings/profile
```

Without the `/a/` prefix, a complete page is returned:

```
$ curl --cookie a http://127.0.0.1:7249/settings/profile
```

Specific fields:

```
$ curl --cookie a http://127.0.0.1:7249/settings/profile/username
$ curl --cookie a http://127.0.0.1:7249/settings/profile/email

```

The following calls show the automatic MIME used by Servant when serving files
directly.

```
$ curl http://127.0.0.1:7249/static/hello.txt
$ curl http://127.0.0.1:7249/static/hello.html
```
