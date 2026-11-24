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

**Not an option:** a variable a function binds for its own purposes. Setting
it globally does not reach the function that binds it.

`clopt.el` (in elisp-extensions) is the Emacs half: it mirrors options as
Customize variables.

Portable Common Lisp, no dependencies. Tests: `(asdf:test-system "clopt")`.
