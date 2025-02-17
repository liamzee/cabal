Commands
========

``cabal help`` groups commands into :ref:`global<command-group-global>`,
:ref:`database<command-group-database>`, :ref:`init<command-group-init>`,
:ref:`configure<command-group-config>`, :ref:`build<command-group-build>`,
:ref:`run<command-group-run>` and :ref:`ship<command-group-ship>` sections.

::

    $ cabal help
    Command line interface to the Haskell Cabal infrastructure.

    See http://www.haskell.org/cabal/ for more information.

    Usage: cabal [GLOBAL FLAGS] [COMMAND [FLAGS]]

    Commands:
     [global]
      user-config            Display and update the user's global cabal configuration.
      help                   Help about commands.
      path                   Display paths used by cabal.

     [package database]
      update                 Updates list of known packages.
      list                   List packages matching a search string.
      info                   Display detailed information about a particular package.

     [initialization and download]
      init                   Create a new cabal package.
      fetch                  Downloads packages for later installation.
      get                    Download/Extract a package's source code (repository).

     [project configuration]
      configure              Add extra project configuration.
      freeze                 Freeze dependencies.
      gen-bounds             Generate dependency bounds.
      outdated               Check for outdated dependencies.

     [project building and installing]
      build                  Compile targets within the project.
      install                Install packages.
      haddock                Build Haddock documentation.
      haddock-project        Generate Haddocks HTML documentation for the cabal project.
      clean                  Clean the package store and remove temporary files.

     [running and testing]
      list-bin               List the path to a single executable.
      repl                   Open an interactive session for the given component.
      run                    Run an executable.
      bench                  Run benchmarks.
      test                   Run test-suites.
      exec                   Give a command access to the store.

     [sanity checks and shipping]
      check                  Check the package for common mistakes.
      sdist                  Generate a source distribution file (.tar.gz).
      upload                 Uploads source packages or documentation to Hackage.
      report                 Upload build reports to a remote server.

     [deprecated]
      unpack                 Deprecated alias for 'get'.
      hscolour               Generate HsColour colourised code, in HTML format.

     [new-style projects (forwards-compatible aliases)]
      Since cabal-install-3.0.0.0, all 'v2-' prefixed names of commands are just
      aliases for the simple unprefixed names.  So v2-build is an alias for
      build, v2-install for install and so on.

     [legacy command aliases]
      No legacy commands are described.

Common Arguments and Flags
--------------------------

Arguments and flags common to some or all commands are:


.. option:: --default-user-config=file

    Allows a "default" ``cabal.config`` freeze file to be passed in
    manually. This file will only be used if one does not exist in the
    project directory already. Typically, this can be set from the
    global cabal ``config`` file so as to provide a default set of
    partial constraints to be used by projects, providing a way for
    users to peg themselves to stable package collections.


.. option:: --allow-newer[=DEPS], --allow-older[=DEPS]

    Selectively relax upper or lower bounds in dependencies without
    editing the package description respectively.

    The following description focuses on upper bounds and the
    :option:`--allow-newer` flag, but applies analogously to
    :option:`--allow-older` and lower bounds. :option:`--allow-newer`
    and :option:`--allow-older` can be used at the same time.

    If you want to install a package A that depends on B >= 1.0 && <
    2.0, but you have the version 2.0 of B installed, you can compile A
    against B 2.0 by using ``cabal install --allow-newer=B A``. This
    works for the whole package index: if A also depends on C that in
    turn depends on B < 2.0, C's dependency on B will be also relaxed.

    Example:

    ::

        $ cd foo
        $ cabal configure
        Resolving dependencies...
        cabal: Could not resolve dependencies:
        [...]
        $ cabal configure --allow-newer
        Resolving dependencies...
        Configuring foo...

    Additional examples:

    ::

        # Relax upper bounds in all dependencies.
        $ cabal install --allow-newer foo

        # Relax upper bounds only in dependencies on bar, baz and quux.
        $ cabal install --allow-newer=bar,baz,quux foo

        # Relax the upper bound on bar and force bar==2.1.
        $ cabal install --allow-newer=bar --constraint="bar==2.1" foo

    It's also possible to limit the scope of :option:`--allow-newer` to single
    packages with the ``--allow-newer=scope:dep`` syntax. This means
    that the dependency on ``dep`` will be relaxed only for the package
    ``scope``.

    Example:

    ::

        # Relax upper bound in foo's dependency on base; also relax upper bound in
        # every package's dependency on lens.
        $ cabal install --allow-newer=foo:base,lens

        # Relax upper bounds in foo's dependency on base and bar's dependency
        # on time; also relax the upper bound in the dependency on lens specified by
        # any package.
        $ cabal install --allow-newer=foo:base,lens --allow-newer=bar:time

    Finally, one can enable :option:`--allow-newer` permanently by setting
    ``allow-newer: True`` in the :ref:`config file <config-file-discovery>`. Enabling
    'allow-newer' selectively is also supported in the config file
    (``allow-newer: foo, bar, baz:base``).

.. option:: --preference=CONSTRAINT

    Specify a soft constraint on versions of a package. The solver will
    attempt to satisfy these preferences on a "best-effort" basis.

.. option:: --enable-build-info

    Generate accurate build information for build components.

    Information contains meta information, such as component type, compiler type, and
    Cabal library version used during the build, but also fine grained information,
    such as dependencies, what modules are part of the component, etc...

    On build, a file ``build-info.json`` (in the ``json`` format) will be written to
    the root of the build directory.

    .. note::
        The format and fields of the generated build information is currently
        experimental. In the future we might add or remove fields, depending
        on the needs of other tooling.

    .. code-block:: json

        {
            "cabal-lib-version": "<cabal lib version>",
            "compiler": {
                "flavour": "<compiler name>",
                "compiler-id": "<compiler id>",
                "path": "<absolute path of the compiler>"
            },
            "components": [
                {
                "type": "<component type, e.g. lib | bench | exe | flib | test>",
                "name": "<component name>",
                "unit-id": "<unitid>",
                "compiler-args": [
                    "<compiler args necessary for compilation>"
                ],
                "modules": [
                    "<modules in this component>"
                ],
                "src-files": [
                    "<source files relative to hs-src-dirs>"
                ],
                "hs-src-dirs": [
                    "<source directories of this component>"
                ],
                "src-dir": "<root directory of this component>",
                "cabal-file": "<cabal file location>"
                }
            ]
        }

    .. jsonschema:: ./json-schemas/build-info.schema.json

.. option:: --disable-build-info

    (default) Do not generate detailed build information for built components.

    Already generated `build-info.json` files will be removed since they would be stale otherwise.

Target Forms
------------

A cabal command target can take any of the following forms:

-  A package target: ``[pkg:]package``, which specifies that all enabled
   components of a package to be built. By default, test suites and
   benchmarks are *not* enabled, unless they are explicitly requested
   (e.g., via ``--enable-tests``.)

-  A component target: ``[package:][ctype:]component``, which specifies
   a specific component (e.g., a library, executable, test suite or
   benchmark) to be built.

-  All packages: ``all``, which specifies all packages within the project.

-  Components of a particular type: ``package:ctypes``, ``all:ctypes``:
   which specifies all components of the given type. Where valid
   ``ctypes`` are:

     - ``libs``, ``libraries``,
     - ``flibs``, ``foreign-libraries``,
     - ``exes``, ``executables``,
     - ``tests``,
     - ``benches``, ``benchmarks``.

-  A module target: ``[package:][ctype:]module``, which specifies that the
   component of which the given module is a part of will be built.

-  A filepath target: ``[package:][ctype:]filepath``, which specifies that the
   component of which the given filepath is a part of will be built.

-  A script target: ``path/to/script``, which specifies the path to a script
   file. This is supported by ``build``, ``repl``, ``run``, and ``clean``.
   Script targets are not part of a package.

.. _command-group-global:

Global commands
---------------

cabal user-config
^^^^^^^^^^^^^^^^^

``cabal user-config [init|diff|update]`` prints and updates user's global
cabal preferences. It is very useful when you are e.g. first configuring
``cabal`` on a new machine.

- ``cabal user-config init`` creates a new configuration file.

  .. option:: --config-file=PATH

      Specify config file path. (default: ``~/.cabal/config``).

  .. option:: -f, --force

    Force configuration file overwriting if already exists.

- ``cabal user-config diff`` prints a diff of the user's config file and the
  default one.

- ``cabal user-config update`` updates the user's config file with additional
  lines.

  .. option:: -a CONFIGLINE or -aCONFIGLINE, --augment=CONFIGLINE

      Pass additional configuration lines to be incorporated in the
      config file. e.g.
      ``cabal user-config update --augment="offline: True"``.

      Note how ``--augment`` syntax follows ``cabal user-config diff``
      output.

cabal path
^^^^^^^^^^

``cabal path`` prints the file system paths used by ``cabal`` for
cache, store, installed binaries, and so on. When run without any
options, it will show all paths, labeled with how they are namen in
the configuration file:

::
   $ cabal path
   cache-dir: /home/haskell/.cache/cabal/packages
   logs-dir: /home/haskell/.cache/cabal/logs
   store-dir: /home/haskell/.local/state/cabal/store
   config-file: /home/haskell/.config/cabal/config
   installdir: /home/haskell/.local/bin
   ...

If ``cabal path`` is passed a single option naming a path, then that
path will be printed *without* any label:

::

   $ cabal path --installdir
   /home/haskell/.local/bin

This is a stable interface and is intended to be used for scripting.
For example:

::
   $ ls $(cabal path --installdir)
   ...

.. _command-group-database:

Package database commands
-------------------------

cabal update
^^^^^^^^^^^^

``cabal update`` updates the state of the package index. If the
project contains multiple remote package repositories it will update
the index of all of them (e.g. when using overlays).

Some examples:

::

    $ cabal update                  # update all remote repos
    $ cabal update head.hackage     # update only head.hackage

cabal list
^^^^^^^^^^

``cabal list [FLAGS] STRINGS`` lists all packages matching a search string.

.. option::  --installed

    Only output installed packages.

.. option::  --simple-output

    Print matching packages in a one-package-one-line format.

.. option::  -i, --ignore-case

.. option::  -I, --strict-case

.. option:: --package-db=DB

    Append the given package database to the list of used package
    databases. See `cabal info`_ for a thorough explanation.

.. option:: -w PATH or -wPATH, --with-compiler=PATH

    Path to specific compiler.

cabal info
^^^^^^^^^^

``cabal info [FLAGS] PACKAGES`` displays useful informations about remote
packages.

.. option:: --package-db=DB

    Append the given package database to the list of package databases
    used (to satisfy dependencies and register into). May be a specific
    file, ``global`` or ``user``. The initial list is ``['global'], ['global',
    'user']``, depending on context. Use ``clear`` to reset the list to empty.

.. _command-group-init:

Initialization and download
---------------------------

cabal init
^^^^^^^^^^

``cabal init [FLAGS]`` initialises a Cabal package, picking
reasonable defaults. Run it in your project folder.

.. option:: -i, --interactive

    Enable interactive mode.

.. option:: -m, --minimal

    Generate a short .cabal file, without extra empty fields or
    explanatory comments.

See :ref:`init quickstart` for an overview on the command, and
``cabal init --help`` for the complete list of options.

cabal fetch
^^^^^^^^^^^

*☞ N.B.:* ``cabal fetch`` only works for legacy ``v1-`` commands and only
for single package projects. If you are not maintaining an old project,
`cabal build`_ with ``--only-download`` has similar effects to ``fetch``
and benefits from compatibility with newer build methods.

``cabal fetch [FLAGS] PACKAGES`` downloads packages for later installation.
It fetches the project plus its dependencies, very useful when
e.g. you plan to work on a project with unreliable or no internet access.

.. option:: --no-dependencies

    Ignore dependencies.

.. option:: --disable-tests

    Disable dependency checking and compilation
    for test suites listed in the package
    description file.

.. option::  --disable-benchmarks

    Disable dependency checking and compilation
    for benchmarks listed in the package
    description file.

Check ``cabal fetch --help`` for a complete list of options.

cabal get
^^^^^^^^^

``cabal get [FLAGS] [PACKAGES]`` (synonym: ``cabal unpack``) downloads and unpacks
the source code of ``PACKAGES`` locally. By default the content of the
packages is unpacked in the current working directory, in named subfolders
(e.g.  ``./filepath-1.2.0.8/``), use ``--destdir=PATH`` to specify another
folder. By default the latest version of the package is downloaded, you can
ask for a spefic one by adding version numbers
(``cabal get random-1.0.0.1``).

The ``cabal get`` command supports the following options:

.. option:: -s[[head|this|...]], --source-repository[=[head|this|...]]

    Clone the package's source repository (Darcs, Git, etc.)
    instead of downloading the tarball. Only works if the
    package specifies a ``source-repository``.

.. option:: --index-state=STATE

    Pin your request to a specific Hackage index state. Available
    ``STATE`` formats: Unix timestamps (e.g. ``@1474732068``),
    ISO8601 UTC timestamps (e.g. ``2016-09-24T17:47:48Z``), or ``HEAD``
    (default).
    This determines which package versions are available as well as which
    ``.cabal`` file revision is selected (unless ``--pristine`` is used).

.. option:: --pristine

    Unpacks the pristine tarball, i.e. disregarding any Hackage revision.

.. option:: -d, --destdir=PATH

    Where to place the package source, defaults to (a subdirectory of)
    the current directory.

.. option:: --only-package-description, --package-description-only

    Unpack the original pristine tarball, rather than updating the
    ``.cabal`` file with the latest revision from the package archive.


.. _command-group-config:

Project configuration
---------------------

cabal configure
^^^^^^^^^^^^^^^

``cabal configure`` takes a set of arguments and writes a
``cabal.project.local`` file based on the flags passed to this command.
``cabal configure FLAGS; cabal build`` is roughly equivalent to
``cabal build FLAGS``, except that with ``configure`` the flags
are persisted to all subsequent calls to ``build``.

``cabal configure`` is intended to be a convenient way to write out
a ``cabal.project.local`` for simple configurations; e.g.,
``cabal configure -w ghc-7.8`` would ensure that all subsequent
builds with ``cabal build`` are performed with the compiler
``ghc-7.8``. For more complex configuration, we recommend writing the
``cabal.project.local`` file directly (or placing it in
``cabal.project``!)

``cabal configure`` inherits options from ``Cabal``. semantics:

-  Any flag accepted by ``./Setup configure``.

-  Any flag accepted by ``cabal configure`` beyond
   ``./Setup configure``, namely ``--cabal-lib-version``,
   ``--constraint``, ``--preference`` and ``--solver.``

-  Any flag accepted by ``cabal install`` beyond ``./Setup configure``.

-  Any flag accepted by ``./Setup haddock``.

The options of all of these flags apply only to *local* packages in a
project; this behavior is different than that of ``cabal install``,
which applies flags to every package that would be built. The motivation
for this is to avoid an innocuous addition to the flags of a package
resulting in a rebuild of every package in the store (which might need
to happen if a flag actually applied to every transitive dependency). To
apply options to an external package, use a ``package`` stanza in a
``cabal.project`` file.

There are two ways of modifying the ``cabal.project.local`` file through
``cabal configure``, either by appending new configurations to it, or
by simply overwriting it all. Overwriting is the default behaviour, as
such, there's a flag ``--enable-append`` to append the new configurations
instead. Since overwriting is rather destructive in nature, a backup system
is in place, which moves the old configuration to a ``cabal.project.local~``
file, this feature can also be disabled by using the ``--disable-backup``
flag.

cabal freeze
^^^^^^^^^^^^

If a package is built in several different environments, such as a
development environment, a staging environment and a production
environment, it may be necessary or desirable to ensure that the same
dependency versions are selected in each environment. This can be done
with the ``freeze`` command:

``cabal freeze`` writes out a **freeze file** which records all of
the versions and flags that are picked by the solver under the
current index and flags.  Default name of this file is
``cabal.project.freeze`` but in combination with a
``--project-file=my.project`` flag (see :ref:`project-file
<cmdoption-project-file>`)
the name will be ``my.project.freeze``.
A freeze file has the same syntax as ``cabal.project`` and looks
something like this:

.. highlight:: cabal

::

    constraints: HTTP ==4000.3.3,
                 HTTP +warp-tests -warn-as-error -network23 +network-uri -mtl1 -conduit10,
                 QuickCheck ==2.9.1,
                 QuickCheck +templatehaskell,
                 -- etc...


For end-user executables, it is recommended that you distribute the
``cabal.project.freeze`` file in your source repository so that all
users see a consistent set of dependencies. For libraries, this is not
recommended: users often need to build against different versions of
libraries than what you developed against.

cabal gen-bounds
^^^^^^^^^^^^^^^^

``cabal gen-bounds [FLAGS]`` generates bounds for all dependencies that do not
currently have them.  Generated bounds are printed to stdout. You can then
paste them into your .cabal file.
The generated bounds conform to the `Package Versioning Policy`_, which is
a recommended versioning system for publicly released Cabal packages.

.. code-block:: console

    $ cabal gen-bounds

For example, given the following dependencies without bounds specified in
:pkg-field:`build-depends`:

::

    build-depends:
      base,
      mtl,
      transformers,

``gen-bounds`` might suggest changing them to the following:

::

    build-depends:
      base          >= 4.15.0 && < 4.16,
      mtl           >= 2.2.2 && < 2.3,
      transformers  >= 0.5.6 && < 0.6,


cabal outdated
^^^^^^^^^^^^^^

``cabal outdated [FLAGS]`` checks for outdated dependencies in the package
description file or freeze file.

Manually updating dependency version bounds in a ``.cabal`` file or a
freeze file can be tedious, especially when there's a lot of
dependencies. The ``cabal outdated`` command is designed to help with
that. It will print a list of packages for which there is a new
version on Hackage that is outside the version bound specified in the
``build-depends`` field. The ``outdated`` command can also be
configured to act on the freeze file and
ignore major (or all) version bumps on Hackage for a subset of
dependencies.

Examples:

.. code-block:: console

    $ cd /some/package
    $ cabal outdated
    Outdated dependencies:
    haskell-src-exts <1.17 (latest: 1.19.1)
    language-javascript <0.6 (latest: 0.6.0.9)
    unix ==2.7.2.0 (latest: 2.7.2.1)

    $ cabal outdated --simple-output
    haskell-src-exts
    language-javascript
    unix

    $ cabal outdated --ignore=haskell-src-exts
    Outdated dependencies:
    language-javascript <0.6 (latest: 0.6.0.9)
    unix ==2.7.2.0 (latest: 2.7.2.1)

    $ cabal outdated --ignore=haskell-src-exts,language-javascript,unix
    All dependencies are up to date.

    $ cabal outdated --ignore=haskell-src-exts,language-javascript,unix -q
    $ echo $?
    0

    $ cd /some/other/package
    $ cabal outdated --freeze-file
    Outdated dependencies:
    HTTP ==4000.3.3 (latest: 4000.3.4)
    HUnit ==1.3.1.1 (latest: 1.5.0.0)

    $ cabal outdated --freeze-file --ignore=HTTP --minor=HUnit
    Outdated dependencies:
    HUnit ==1.3.1.1 (latest: 1.3.1.2)


``cabal outdated`` supports the following flags:

.. option:: --freeze-file

    Read dependency version bounds from the freeze file.

    (``cabal.config``) instead of the package description file
    (``$PACKAGENAME.cabal``).

.. option:: --v2-freeze-file

    :since: 2.4

    Read dependency version bounds from the v2-style freeze file
    (by default, ``cabal.project.freeze``) instead of the package
    description file. ``--new-freeze-file`` is an alias for this flag
    that can be used with pre-2.4 ``cabal``.

.. option:: --project-file=FILE

    :since: 2.4

    Read dependency version bounds from the v2-style freeze file
    related to the named project file (i.e., ``$PROJECTFILE.freeze``)
    instead of the package description file. If multiple ``--project-file``
    flags are provided, only the final one is considered. This flag
    must only be passed in when ``--new-freeze-file`` is present.

.. option:: --simple-output

    Print only the names of outdated dependencies, one per line.

.. option:: --exit-code

    Exit with a non-zero exit code when there are outdated dependencies.

.. option:: -q, --quiet

    Don't print any output. Implies ``-v0`` and ``--exit-code``.

.. option:: --ignore=PKGS

    Don't warn about outdated dependency version bounds for the packages in this list.

.. option:: --minor[PKGS]

    Ignore major version bumps for these packages.

    E.g. if there's a version 2.0 of a package ``pkg`` on Hackage and the freeze
    file specifies the constraint ``pkg == 1.9``, ``cabal outdated --freeze
    --minor=pkg`` will only consider the ``pkg`` outdated when there's a version
    of ``pkg`` on Hackage satisfying ``pkg > 1.9 && < 2.0``. ``--minor`` can also
    be used without arguments, in that case major version bumps are ignored for
    all packages.

.. _command-group-build:

Project building and installing
-------------------------------

cabal build
^^^^^^^^^^^

``cabal build`` takes a set of targets and builds them. It
automatically handles building and installing any dependencies of these
targets.

In component targets, ``package:`` and ``ctype:`` (valid component types
are ``lib``, ``flib``, ``exe``, ``test`` and ``bench``) can be used to
disambiguate when multiple packages define the same component, or the
same component name is used in a package (e.g., a package ``foo``
defines both an executable and library named ``foo``). We always prefer
interpreting a target as a package name rather than as a component name.

Some example targets:

::

    $ cabal build lib:foo-pkg       # build the library named foo-pkg
    $ cabal build foo-pkg:foo-tests # build foo-tests in foo-pkg
    $ cabal build src/Lib.s         # build the library component to
                                       # which "src/Lib.hs" belongs
    $ cabal build app/Main.hs       # build the executable component of
                                       # "app/Main.hs"
    $ cabal build Lib               # build the library component to
                                       # which the module "Lib" belongs
    $ cabal build path/to/script    # build the script as an executable

Beyond a list of targets, ``cabal build`` accepts all the flags that
``cabal configure`` takes. Most of these flags are only taken into
consideration when building local packages; however, some flags may
cause extra store packages to be built (for example,
``--enable-profiling`` will automatically make sure profiling libraries
for all transitive dependencies are built and installed.)

When building a script, the executable is cached under the cabal directory.
See ``cabal run`` for more information on scripts.

In addition ``cabal build`` accepts these flags:

.. option:: --only-configure

    When given we will forego performing a full build and abort after running
    the configure phase of each target package.

cabal install
^^^^^^^^^^^^^

``cabal install [FLAGS] [TARGETS]`` builds the specified target packages and
symlinks/copies their executables in ``installdir`` (usually ``~/.local/bin``).

.. warning::

  If not every package has an executable to install, use ``all:exes`` rather
  than ``all`` as the target. To overwrite an installation, use
  ``--overwrite-policy=always`` as the default policy is ``never``.

For example this command will build the latest ``cabal-install`` and symlink
its ``cabal`` executable:

::

    $ cabal install cabal-install

In addition, it's possible to use ``cabal install`` to install components
of a local project. For example, with an up-to-date Git clone of the Cabal
repository, this command will build cabal-install HEAD and symlink the
``cabal`` executable:

::

    $ cabal install exe:cabal

Where symlinking is not possible (eg. on some Windows versions) the ``copy``
method is used by default. You can specify the install method
by using ``--install-method`` flag:

::

    $ cabal install exe:cabal --install-method=copy --installdir=$HOME/bin

Note that copied executables are not self-contained, since they might use
data-files from the store.

.. _adding-libraries:

Adding libraries to GHC package environments
""""""""""""""""""""""""""""""""""""""""""""

It is also possible to "install" libraries using the ``--lib`` flag. For
example, this command will build the latest Cabal library and install it:

::

    $ cabal install --lib Cabal

This works by managing GHC package environment files. By default, it is writing
to the global environment in ``~/.ghc/$ARCH-$OS-$GHCVER/environments/default``.
``install`` provides the ``--package-env`` flag to control which of these
environments is modified.

This command will modify the environment file in the current directory:

::

    $ cabal install --lib Cabal --package-env .

This command will modify the environment file in the ``~/foo`` directory:

::

    $ cabal install --lib Cabal --package-env foo/

Do note that the results of the previous two commands will be overwritten by
the use of other style commands, so it is not recommended to use them inside
a project directory.

This command will modify the environment in the ``local.env`` file in the
current directory:

::

    $ cabal install --lib Cabal --package-env local.env

This command will modify the ``myenv`` named global environment:

::

    $ cabal install --lib Cabal --package-env myenv

If you wish to create a named environment file in the current directory where
the name does not contain an extension, you must reference it as ``./myenv``.

You can learn more about how to use these environments in `this section of the
GHC manual <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-environments>`_.

cabal haddock
^^^^^^^^^^^^^

``cabal haddock [FLAGS] [TARGET]`` builds Haddock documentation for
the specified packages within the project.

If a target is not a library :cfg-field:`haddock-benchmarks`,
:cfg-field:`haddock-executables`, :cfg-field:`haddock-internal`,
:cfg-field:`haddock-tests` will be implied as necessary.

cabal haddock-project
^^^^^^^^^^^^^^^^^^^^^

``cabal haddock-project [FLAGS]`` builds Haddock documentation for all local
packages specified in the project.

By default the documentation will be put in ``./haddocks`` folder, this can be
modified with the ``--output`` flag.

This command supports two primary modes: building a self contained directory
(which is the default mode) or documentation that links to hackage (with
``--hackage`` flag).

In both cases the html index as well as quickjump index will include all terms
and types defined in any of the local packages, but not ones that are included
in any of the dependencies.  But note that if you navigate to a dependency,
you will have access to its quickjump index.

The generated landing page will contain one tree of all modules per local
package.

cabal clean
^^^^^^^^^^^

``cabal clean [FLAGS]`` cleans up the temporary files and build artifacts
stored in the ``dist-newstyle`` folder.

By default, it removes the entire folder, but it can also spare the configuration
and caches if the ``--save-config`` option is given, in which case it only removes
the build artefacts (``.hi``, ``.o`` along with any other temporary files generated
by the compiler, along with the build output).

``cabal clean [FLAGS] path/to/script`` cleans up the temporary files and build
artifacts for the script, which are stored under the .cabal/script-builds directory.

In addition when clean is invoked it will remove all script build artifacts for
which the corresponding script no longer exists.

.. _command-group-run:

Running and testing
-------------------

cabal list-bin
^^^^^^^^^^^^^^

``cabal list-bin`` will either (a) display the path for a single executable or (b)
complain that the target doesn't resolve to a single binary. In the latter case,
it will name the binary products contained in the package. These products can
be used to narrow the search and get an actual path to a particular executable.

Example showing a failure to resolve to a single executable.

::

    $ cabal list-bin cabal-install
    cabal: The list-bin command is for finding a single binary at once. The
    target 'cabal-install' refers to the package cabal-install-#.#.#.# which
    includes the executable 'cabal', the test suite 'unit-tests', the test suite
    'mem-use-tests', the test suite 'long-tests' and the test suite
    'integration-tests2'.

For a scope that results in only one item we'll get a path.

::

    $ cabal list-bin cabal-install:exes
    /.../dist-newstyle/build/.../cabal/cabal

    $ cabal list-bin cabal-install:cabal
    /.../dist-newstyle/build/.../cabal/cabal

We can also scope to test suite targets as they produce binaries.

::

    $ cabal list-bin cabal-install:tests
    cabal: The list-bin command is for finding a single binary at once. The
    target 'cabal-install:tests' refers to the test suites in the package
    cabal-install-#.#.#.# which includes the test suite 'unit-tests', the test
    suite 'mem-use-tests', the test suite 'long-tests' and the test suite
    'integration-tests2'.

    $ cabal list-bin cabal-install:unit-tests
    /.../dist-newstyle/.../unit-tests/unit-tests

Note that ``cabal list-bin`` will print the executables' location, but
will not make sure that these executables actually exist (i.e., have
been successfully built).  In order to determine the correct location,
it may invoke the configuration step (see ``cabal configure``).

cabal repl
^^^^^^^^^^

``cabal repl TARGET [FLAGS]``
opens an interactive session for the target component within the project and
loads all of the modules of the target into GHCi as interpreted bytecode.
The available targets are the same as for the ``build`` command: individual components
within packages in the project, including libraries, executables, test-suites
and benchmarks (see `the build section <#cabal-build>`__ for the target syntax).
Local packages can also be specified, in which case the library
component in the package will be used, or the (first listed) executable in the
package if there is no library. Dependencies are built or rebuilt as necessary.

Examples:

::

    $ cabal repl                # default component in the package in the current directory
    $ cabal repl pkgname        # default component in the package named 'pkgname'
    $ cabal repl ./pkgfoo       # default component in the package in the ./pkgfoo directory
    $ cabal repl cname          # component named 'cname'
    $ cabal repl pkgname:cname  # component 'cname' in the package 'pkgname'

Configuration flags can be specified on the command line and these extend the project
configuration from the 'cabal.project', 'cabal.project.local' and other files.

.. option:: --repl-options=FLAG

    To avoid ``ghci``-specific flags from triggering unneeded global rebuilds, these
    flags are stripped from the internal configuration. As a result,
    ``--ghc-options`` will no longer (reliably) work to pass flags to ``ghci`` (or
    other REPLs). Instead, you should use the ``--repl-options`` flag to
    specify these options to the invoked REPL.

.. option:: --repl-no-load

    Disables the loading of target modules at startup.

.. option:: -b DEPENDENCIES or -bDEPENDENCIES, --build-depends=DEPENDENCIES

    A way to experiment with libraries without needing to download
    them manually or to install them globally.

    This command opens a REPL with the current default target loaded, and a version
    of the ``vector`` package matching that specification exposed.

    ::

        $ cabal repl --build-depends="vector >= 0.12 && < 0.13"

    Both of these commands do the same thing as the above, but only expose ``base``,
    ``vector``, and the ``vector`` package's transitive dependencies even if the user
    is in a project context.

    ::

        $ cabal repl --ignore-project --build-depends="vector >= 0.12 && < 0.13"
        $ cabal repl --project='' --build-depends="vector >= 0.12 && < 0.13"

    This command would add ``vector``, but not (for example) ``primitive``, because
    it only includes the packages specified on the command line (and ``base``, which
    cannot be excluded for technical reasons).

    ::

        $ cabal repl --build-depends=vector --no-transitive-deps

``cabal repl`` can open scripts by passing the path to the script as the target.

::

    $ cabal repl path/to/script

The configuration information for the script is cached under the cabal directory
and can be pre-built with ``cabal build path/to/script``.
See ``cabal run`` for more information on scripts.

.. option:: --enable-multi-repl

    Allow starting GHCi with multiple targets.
    This requires GHC with multiple home unit support (GHC-9.4+)

    The closure of required components will be loaded.

.. option:: --disable-multi-repl

    Disallow starting GHCi with multiple targets. This reverts back to the behaviour
    in version 3.10 and earlier where only a single component can be loaded at
    once.

.. _cabal run:

cabal run
^^^^^^^^^

``cabal run [TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]`` runs the executable
specified by the target, which can be a component, a package or can be left
blank, as long as it can uniquely identify an executable within the project.
Tests and benchmarks are also treated as executables.

See `the build section <#cabal-build>`__ for the target syntax.

When ``TARGET`` is one of the following:

- A component target: execute the specified executable, benchmark or test suite.

- A package target:
   1. If the package has exactly one executable component, it will be selected.
   2. If the package has multiple executable components, an error is raised.
   3. If the package has exactly one test or benchmark component, it will be selected.
   4. Otherwise an issue is raised.

- The path to a script: execute the script at the path.

- Empty target: Same as package target, implicitly using the package from the current
  working directory.

Except in the case of the empty target, the strings after it will be
passed to the executable as arguments.

If one of the arguments starts with ``-`` it will be interpreted as
a cabal flag, so if you need to pass flags to the executable you
have to separate them with ``--``.

::

    $ cabal run target -- -a -bcd --argument

``run`` supports running script files that use a certain format.
Scripts look like:

::

    #!/usr/bin/env cabal
    {- cabal:
    build-depends: base ^>= 4.14
                , shelly ^>= 1.10
    -}
    {- project:
    with-compiler: ghc-8.10.7
    -}

    main :: IO ()
    main = do
        ...

Where there cabal metadata block is mandatory and contains fields from a
package executable block, and the project metadata block is optional and
contains fields that would be in the cabal.project file in a regular project.

Only some fields are supported in the metadata blocks, and these fields are
currently not validated. See
`#8024 <https://github.com/haskell/cabal/issues/8024>`__ for details.

A script can either be executed directly using `cabal` as an interpreter or
with the command:

::

    $ cabal run path/to/script

The executable is cached under the cabal directory, and can be pre-built with
``cabal build path/to/script`` and the cache can be removed with
``cabal clean path/to/script``.

A note on targets: Whenever a command takes a script target and it matches the
name of another target, the other target is preferred. To load the script
instead pass it as an explicit path: ./script

By default, scripts are run at silent verbosity (``--verbose=0``). To show the
build output for a script either use the command

::

    $ cabal run --verbose=n path/to/script

or the interpreter line

::

    #!/usr/bin/env -S cabal run --verbose=n

For more information see :cfg-field:`verbose`

cabal bench
^^^^^^^^^^^

``cabal bench [TARGETS] [FLAGS]`` runs the specified benchmarks
(all the benchmarks in the current package by default), first ensuring
they are up to date.

``cabal bench`` inherits flags of the ``bench`` subcommand of ``Setup.hs``,
:ref:`see the corresponding section <setup-bench>`.

cabal test
^^^^^^^^^^

``cabal test [TARGETS] [FLAGS]`` runs the specified test suites
(all the test suites in the current package by default), first ensuring
they are up to date.

``cabal test`` inherits flags of the ``test`` subcommand of ``Setup.hs``
(:ref:`see the corresponding section <setup-test>`) with one caveat: every
``Setup.hs test`` flag receives the ``test-`` prefix if it already does
not have one; e.g. ``--show-details`` becomes ``--test-show-details`` but
``--test-wrapper`` remains the same.

cabal exec
^^^^^^^^^^

``cabal exec [FLAGS] [--] COMMAND [--] [ARGS]`` runs the specified command
using the project's environment. That is, passing the right flags to compiler
invocations and bringing the project's executables into scope.

.. _command-group-ship:

Sanity checks and shipping
--------------------------

cabal check
^^^^^^^^^^^

``cabal check [FLAGS]`` checks the package for common mistakes (e.g.: if
it is missing important fields like ``synopsis``, if it is using
tricky GHC options, etc.).

Run ``cabal check`` in the folder where your ``.cabal`` package file is.

.. option:: -v[n], --verbose[=n]

    Control verbosity (n is 0--3, default verbosity level is 1).

Issues are classified as ``Warning``\s and ``Error``\s. The latter correspond
to Hackage requirements for uploaded packages: if no error is reported,
Hackage should accept your package. If errors are present ``cabal check``
exits with ``1`` and Hackage will refuse the package.

A list of all warnings with their constructor:

- ParseWarning: warnings inherited from parser.
- NoNameField: missing ``name`` field.
- NoVersionField: missing ``version`` field.
- NoTarget: missing target in ``.cabal``.
- UnnamedInternal: unnamed internal library.
- DuplicateSections: duplicate name in target.
- IllegalLibraryName: internal library with same name as package.
- NoModulesExposed: no module exposed in library.
- SignaturesCabal2: ``signatures`` used with ``cabal-version`` < 2.0
- AutogenNotExposed: ``autogen-module`` neither in ``exposed-modules`` nor ``other-modules``.
- AutogenIncludesNotIncluded: ``autogen-include`` neither in ``include`` nor ``install-includes``.
- NoMainIs: missing ``main-is``.
- NoHsLhsMain: ``main-is`` is not ``.hs`` nor ``.lhs``.
- MainCCabal1_18: C-like source file in ``main-is`` with ``cabal-version`` < 1.18.
- AutogenNoOther: ``autogen-module`` not in ``other-modules``.
- AutogenIncludesNotIncludedExe: ``autogen-include`` not in ``includes``.
- TestsuiteTypeNotKnown: unknown test-suite type.
- TestsuiteNotSupported: unsupported test-suite type.
- BenchmarkTypeNotKnown: unknown benchmark type.
- BenchmarkNotSupported: unsupported benchmark type.
- NoHsLhsMainBench: ``main-is`` for benchmark is neither ``.hs`` nor ``.lhs``.
- InvalidNameWin: invalid package name on Windows.
- ZPrefix: package with ``z-`` prexif (reseved for Cabal.
- NoBuildType: missing ``build-type``.
- NoCustomSetup: ``custom-setup`` section without ``build-type: Custom``
- UnknownCompilers: unknown compiler in ``tested-with``.
- UnknownLanguages: unknown languages.
- UnknownExtensions: unknown extensions.
- LanguagesAsExtension: languages listed as extensions.
- DeprecatedExtensions: deprecated extensions.
- MissingField: missing cabal field (one of ``category``, ``maintainer``, ``synopsis``, ``description``).
- SynopsisTooLong: ``synopsis`` longer than 80 characters.
- ShortDesc: ``description`` shorter than ``synopsis``.
- InvalidTestWith: invalid ``tested-with`` version range.
- ImpossibleInternalDep: impossible internal library version range dependency.
- ImpossibleInternalExe: impossible internal executable version range dependency.
- MissingInternalExe: missing internal executable.
- NONELicense: ``NONE`` in ``license`` field.
- NoLicense: no ``license`` field.
- AllRightsReservedLicense: all rights reserved license.
- LicenseMessParse: license not to be used with `cabal-version` < 1.4.
- UnrecognisedLicense: unknown license.
- UncommonBSD4: uncommon BSD (BSD4) license.
- UnknownLicenseVersion: unknown license version.
- NoLicenseFile: missing license file.
- UnrecognisedSourceRepo: unrecognised kind of source-repository.
- MissingType: missing ``type`` in ``source-repository``.
- MissingLocation: missing ``location`` in ``source-repository``.
- MissingModule: missing ``module`` in ``source-repository``.
- MissingTag: missing ``tag`` in ``source-repository``.
- SubdirRelPath: ``subdir`` in ``source-repository`` must be relative.
- SubdirGoodRelPath: malformed ``subdir`` in ``source-repository``.
- OptFasm: unnecessary ``-fasm``.
- OptViaC: unnecessary ``-fvia-C``.
- OptHpc: unnecessary ``-fhpc``.
- OptProf: unnecessary ``-prof``.
- OptO: unnecessary ``-o``.
- OptHide: unnecessary ``-hide-package``.
- OptMake: unnecessary ``--make``.
- OptONot: unnecessary disable optimisation flag.
- OptOOne: unnecessary optimisation flag (``-O1``).
- OptOTwo: unnecessary optimisation flag (``-O2``).
- OptSplitSections: unnecessary ``-split-section``.
- OptSplitObjs: unnecessary ``-split-objs``.
- OptWls: unnecessary ``-optl-Wl,-s``.
- OptExts: use ``extension`` field instead of ``-fglasgow-exts``.
- OptRts: unnecessary ``-rtsopts``.
- OptWithRts: unnecessary ``-with-rtsopts``.
- COptONumber: unnecessary ``-O[n]`` in C code.
- COptCPP: unportable ``-cpp-options`` flag.
- OptAlternatives: C-like options in wrong cabal field.
- RelativeOutside: relative path outside of source tree.
- AbsolutePath: absolute path where not allowed.
- BadRelativePath: malformed relative path.
- DistPoint: unreliable path pointing inside ``dist``.
- GlobSyntaxError: glob syntax error.
- RecursiveGlobInRoot: recursive glob including source control folders.
- InvalidOnWin: invalid path on Windows.
- FilePathTooLong: path too long.
- FilePathNameTooLong: path *name* too long (POSIX).
- FilePathSplitTooLong: path non portable (POSIX, split requirements).
- FilePathEmpty: empty path.
- CVTestSuite: ``test-suite`` used with ``cabal-version`` < 1.10.
- CVDefaultLanguage: ``default-language`` used with ``cabal-version`` < 1.10.
- CVDefaultLanguageComponent: missing ``default-language``.
- CVExtraDocFiles: `extra-doc-files` used with ``cabal-version`` < 1.18.
- CVMultiLib: multiple ``library`` sections with ``cabal-version`` < 2.0.
- CVReexported: ``reexported-modules`` with ``cabal-version`` < 1.22.
- CVMixins: ``mixins`` with ``cabal-version`` < 2.0.
- CVExtraFrameworkDirs: ``extra-framework-dirs`` with ``cabal-version`` < 1.24.
- CVDefaultExtensions: ``default-extensions`` with ``cabal-version`` < 1.10.
- CVExtensionsDeprecated: deprecated ``extensions`` field used with ``cabal-version`` ≥ 1.10
- CVSources: ``asm-sources``, ``cmm-sources``, ``extra-bundled-libraries`` or ``extra-library-flavours`` used with ``cabal-version`` < 3.0.
- CVExtraDynamic: ``extra-dynamic-library-flavours`` used with cabal-version < 3.0.
- CVVirtualModules: ``virtual-modules`` used with cabal-version < 2.2.
- CVSourceRepository: ``source-repository`` used with ``cabal-version`` 1.6.
- CVExtensions: incompatible language extension with ``cabal-version``.
- CVCustomSetup: missing ``setup-depends`` field in ``custom-setup`` with ``cabal-version`` ≥ 1.24.
- CVExpliticDepsCustomSetup: missing dependencies in ``custom-setup`` with ``cabal-version`` ≥ 1.24.
- CVAutogenPaths: missing autogen ``Paths_*`` modules in ``autogen-modules`` (``cabal-version`` ≥ 2.0).
- CVAutogenPackageInfo: missing autogen ``PackageInfo_*`` modules in ``autogen-modules`` *and* ``exposed-modules``/``other-modules`` (``cabal-version`` ≥ 2.0).
- GlobNoMatch: glob pattern not matching any file.
- GlobExactMatch: glob pattern not matching any file becuase of lack of extension matching (`cabal-version` < 2.4).
- GlobNoDir: glob pattern trying to match a missing directory.
- UnknownOS: unknown operating system name in condition.
- UnknownArch: unknown architecture in condition.
- UnknownCompiler: unknown compiler in condition.
- BaseNoUpperBounds: missing upper bounds for important dependencies (``base``, and for ``custom-setup`` ``Cabal`` too).
- MissingUpperBounds: missing upper bound in dependency (excluding test-suites and benchmarks).
- SuspiciousFlagName: troublesome flag name (e.g. starting with a dash).
- DeclaredUsedFlags: unused user flags.
- NonASCIICustomField: non-ASCII characters in custom field.
- RebindableClashPaths: ``Rebindable Syntax`` with ``OverloadedStrings``/``OverloadedStrings`` plus autogenerated ``Paths_*`` modules with ``cabal-version`` < 2.2.
- RebindableClashPackageInfo: ``Rebindable Syntax`` with ``OverloadedStrings``/``OverloadedStrings`` plus autogenerated ``PackageInfo_*`` modules with ``cabal-version`` < 2.2.
- WErrorUnneeded: ``-WError`` not under a user flag.
- JUnneeded: suspicious ``-j[n]`` usage.
- FDeferTypeErrorsUnneeded: suspicious ``-fdefer-type-errors``.
- DynamicUnneeded: suspicious ``-d*`` debug flag for distributed package.
- ProfilingUnneeded: suspicious ``-fprof-*`` flag.
- UpperBoundSetup: missing upper bounds in ``setup-depends``.
- DuplicateModule: duplicate modules in target.
- PotentialDupModule: potential duplicate module in target (subject to conditionals).
- BOMStart: unicode byte order mark (BOM) character at start of file.
- NotPackageName: filename not matching ``name``.
- NoDesc: no ``.cabal`` file found in folder.
- MultiDesc: multiple ``.cabal`` files found in folder.
- UnknownFile: path refers to a file which does not exist.
- MissingSetupFile: missing ``Setup.hs`` or ``Setup.lsh``.
- MissingConfigureScript: missing ``configure`` script with ``build-type: Configure``.
- UnknownDirectory: paths refer to a directory which does not exist.
- MissingSourceControl: missing ``source-repository`` section.
- MissingExpectedDocFiles: missing expected documentation files (changelog).
- WrongFieldForExpectedDocFiles: documentation files listed in ``extra-source-files`` instead of ``extra-doc-files``.

cabal sdist
^^^^^^^^^^^

``cabal sdist [FLAGS] [PACKAGES]`` takes the crucial files needed to build ``PACKAGES``
and puts them into an archive format ready for upload to Hackage. These archives are stable
and two archives of the same format built from the same source will hash to the same value.

``cabal sdist`` takes the following flags:

.. option:: -l, --list-only

    Rather than creating an archive, lists files that would be included.

    Output is to ``stdout`` by default. The file paths are relative to the project's root
    directory.

.. option:: -o PATH or -oPATH, --output-directory=PATH

    Sets the output dir, if a non-default one is desired. The default is
    ``dist-newstyle/sdist/``. ``--output-directory -`` will send output to ``stdout``
    unless multiple archives are being created.

.. option:: --null-sep

    Only used with ``--list-only``. Separates filenames with a NUL
    byte instead of newlines.

``sdist`` is inherently incompatible with sdist hooks (which were removed in `Cabal-3.0`),
not due to implementation but due to fundamental core invariants
(same source code should result in the same tarball, byte for byte)
that must be satisfied for it to function correctly in the larger build ecosystem.
``autogen-modules`` is able to replace uses of the hooks to add generated modules, along with
the custom publishing of Haddock documentation to Hackage.

cabal upload
^^^^^^^^^^^^

``cabal upload [FLAGS] TARFILES`` uploads source packages or documentation
to Hackage.

.. option:: --publish

    Publish the package immediately instead of uploading it as a
    `package candidate <https://hackage.haskell.org/upload#candidates>`__
    (make sure everything is fine, you cannot delete published packages
    on Hackage!).

.. option:: -d, --documentation

    Upload documentation instead of a source package. To upload
    documentation for a published package (and not a candidate), add
    ``--publish``.

.. option:: -t TOKEN or -tTOKEN, --token=TOKEN

    Your Hackage authentication token. You can create and delete 
    authentication tokens on Hackage's `account management page 
    <https://hackage.haskell.org/users/account-management>`__.

.. option:: -u USERNAME or -uUSERNAME, --username=USERNAME

    Your Hackage username.

.. option:: -p PASSWORD or -pPASSWORD, --password=PASSWORD

    Your Hackage password.

.. option:: -P COMMAND or -PCOMMAND, --password-command=COMMAND

    Command to get your Hackage password.  Arguments with whitespace
    must be quoted (double-quotes only).  For example:

    ::

        --password-command='sh -c "grep hackage ~/secrets | cut -d : -f 2"'

    Or in the config file:

    ::

        password-command: sh -c "grep hackage ~/secrets | cut -d : -f 2"


cabal report
^^^^^^^^^^^^

``cabal report [FLAGS]`` uploads build reports to Hackage.

.. option:: -t TOKEN or -tTOKEN, --token=TOKEN

    Your Hackage authentication token. You can create and delete 
    authentication tokens on Hackage's `account management page 
    <https://hackage.haskell.org/users/account-management>`__.

.. option:: -u USERNAME or -uUSERNAME, --username=USERNAME

    Your Hackage username.

.. option:: -p PASSWORD or -pPASSWORD, --password=PASSWORD

    Your Hackage password.

.. include:: references.inc
