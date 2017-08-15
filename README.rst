========
idris-go
========

A `Go <https://golang.org/>`_ backend for `Idris <https://www.idris-lang.org/>`_.


What is working
===============

* Calling Go from Idris (see ``examples/echo.idr`` for an echo server)
* Tail calls are implemented with ``goto`` if self-recursive and with a
  trampoline otherwise, hence arbitrary deep tail calls should work.

See the ``examples`` directory for some examples.

What is not working
===================

* Calling Idris from Go
* Not every of Idris `primitive functions` is implemented. The use of an
  unimplemented primitive function will result in a panic at runtime.


Building from source
====================

Easiest with `Stack <https://docs.haskellstack.org/en/stable/README/>`_. `See
their documentation for details how to install it
<https://docs.haskellstack.org/en/stable/install_and_upgrade/>`_.

Assuming you have stack installed, then you can simply do::

   stack build

To run the tests, execute::

   stack test


Translating Idris programs to Go
================================

::

   stack exec idris -- -p go --codegen go examples/hello.idr -o hello.go


License
=======

MIT/Expat. See ``LICENSE`` for details.
