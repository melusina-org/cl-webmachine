# Webmachine – HTTP Semantic Awareness on top of Hunchentoot

Webmachine is an application layer that adds HTTP semantic awareness
on top of the excellent bit-pushing and HTTP syntax-management
provided by Hunchentoot, and provides a simple and clean way to
connect that to your application's behavior.

Webmachine maps the various aspects of the HTTP protocol to generic
methods which are customisable. For instance, content negotiation,
authorization and caching are orthogonally separated and an
application using Webmachine can easily describe desired beahviours in
testable and reusable application TRAITS, then blend them together in
actual web resources.

The design is inspired by the mythic Webmachine of Erlang and honours
it with that name.

*This software is Copyright © 2018–2023 Michaël Le Barbier and is
distributed under the terms described in the LICENSE file. External
contributions distributed in the asset folder are distributed under
their own terms.*


# Alpha Software

This is Alpha software and work in progress and system's interface is
likely to change.  Join the conversation on the home repository if
that happens.

Porting is not complete but authorization and content negotiation are
available.  Webmachine can serve simple web applications already and
an Example is provided.


# Example

The system `org.melusina.webmachine/example` is a simple application
example of a web application written with Webmachine.

~~~ lisp
Cl-USER> (ql:quickload "org.melusina.webmachine/example")
Cl-USER> (webmachine/example:create-users)
Cl-USER> (webmachine/example:start)
~~~

Browse to `http://localhost:8080/about` to interact with the
application.


The systems `org.melusina.webmachine/asset` and
`org.melusina.webmachine/server` contain general functions that can be
interesting to Webmachine users when driving their own experiments.


# Testsuite

The system `org.melusina.webmachine/testsuite` implements a
testsuite. Run it with:

~~~ lisp
Cl-USER> (ql:quickload "org.melusina.webmachine/testsuite")
Cl-USER> (webmachine/testsuite:run-all-tests)
…
Name: ORG.MELUSINA.WEBMACHINE/TESTSUITE:RUN-ALL-TESTS
Total: 159
Success: 159/159 (100%)
Failure: 0/159 (0%)
Condition: 0/159 (0%)
Outcome: Success
~~~

GitHub Actions run the testsuite on every push to the repository.


# Development

The system `org.melusina.webmachine/development` provides developer
tools, most importantly a linter and a file template.

~~~ lisp
Cl-USER> (ql:quickload "org.melusina.webmachine/development")
Cl-USER> (webmachine/development:lint)
~~~


# Join the Conversation

If you are interested in Webmachine, try to use it, want to
contribute, join the conversation!

- [GitHub Issues](https://github.com/melusina-org/cl-webmachine/issues)
- IRC #commonlisp, see [community pointers](https://common-lisp.net/community)


# Other Webmachine Implementations

- [Erlang](https://github.com/webmachine/webmachine)
- [OCaml](https://github.com/inhabitedtype/ocaml-webmachine)
- [Python](https://github.com/benoitc/pywebmachine)
