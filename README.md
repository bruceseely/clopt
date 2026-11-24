# clopt

Declared, documented, typed options for Common Lisp systems.

A Lisp system has many special variables and only a few of them are options a
person should set. `defoption` marks the ones that are: it defines the variable
(as `defvar`, so reloading keeps a value you set) and registers it with its
type and documentation.

```lisp
(clopt:defoption *alignment-strategy* :automatic
  :type (member :automatic :individuals)
  :doc "How two graphs being combined decide which concepts are the same one.")
```

A front end — a settings web page, Emacs Customize — can then list a system's
options, show each docstring, offer the allowed values, and set them safely:

```lisp
(clopt:list-options :system :conceptual-graphs)   ; => (#<OPTION ...> ...)
(clopt:option-doc '*alignment-strategy*)          ; the docstring
(clopt:option-choices '*alignment-strategy*)      ; => (:AUTOMATIC :INDIVIDUALS)
(setf (clopt:option-value '*alignment-strategy*) :individuals)
(setf (clopt:option-value '*alignment-strategy*) :sloppy)  ; INVALID-OPTION-VALUE
(clopt:reset-option '*alignment-strategy*)
```

An option's system defaults to its symbol's package (as a keyword); `:system`
overrides it. An option whose value is not a variable — a slot, a symbol macro
over one — is registered with `register-option` and a `:reader` / `:writer`
pair.

## Saving and loading settings

A settings file records how a configuration differs from the defaults: only
the options not at their default are saved. Loading one sets those and puts
every other registered option back to its default, so a file always gives the
same configuration. Pass `:reset nil` to layer a file on top instead.

```lisp
(setf clopt:*settings-directory* #p"~/.myapp/settings/")
(clopt:save-settings "work")          ; ~/.myapp/settings/work.settings
(clopt:settings-plan "work")          ; what loading would change, without changing it
(clopt:load-settings "work")
(clopt:settings-names)                ; => ("work")
```

The file is Lisp data, never code, read with `*read-eval*` off:

```lisp
(:clopt-settings 1)
("CONCEPTUAL-GRAPHS" "*ALIGNMENT-STRATEGY*" :individuals)
```

Options are named by package and symbol name as strings, so reading a file
interns nothing. An entry for a system that isn't loaded is skipped and kept
when the file is saved over; a value not of its option's type is refused. A
set's name is letters, digits, space, `-` and `_` only, so a name can't reach
outside the directory. Functions take a name or a pathname.

**Not an option:** a variable a function binds for its own purposes. Setting
it globally does not reach the function that binds it.

`clopt.el` (in elisp-extensions) is the Emacs half: it mirrors options as
Customize variables.

Portable Common Lisp, no dependencies. Tests: `(asdf:test-system "clopt")`.
