# sdl2.kit

This is a utility kit for `cl-sdl2` that provides something similar to
`GLUT`.  However, it's also geared at being useful for "real"
applications or games.

At the moment, this includes the following functionality:

* A generalized event loop
* Managing multiple windows
* Managing the rendering loop, including GL
* Managing the "shader dictionary": compiling shaders, linking
  programs, outputting errors, activating programs, and setting
  uniforms by name.

For an example, see `examples/sdl2kit.lisp`.

## Windows

To make a new window, you simply create a subclass of `WINDOW` or
`GL-WINDOW`, and implement the `WINDOW` protocol.  You should
primarily specialize on `WINDOW`:

* `window-event WINDOW TYPE TIMESTAMP DATA1 DATA2`:  Handle a WM event
  (see the SDL2 documentation for parameters)
* `mousebutton-event WINDOW STATE TIMESTAMP BUTTON X Y`
* `mousemotion-event WINDOW TIMESTAMP X Y`
* `textinput-event WINDOW TIMESTAMP TEXT`
* `keyboard-event WINDOW STATE TIMESTAMP REPEAT-P KEYSYM`
* `other-event WINDOW EVENT`: Other events not implemented yet
* `render WINDOW`
* `close-window WINDOW`

**NOTE:** Certain methods *require* you to `CALL-NEXT-METHOD` to
function properly.  This includes your window's
`INITIALIZE-INSTANCE`.  See the example for details.

Of these, you should only call `RENDER` or `CLOSE-WINDOW` from user
code.  However, these can be called from *any* thread safely.

By default, windows do not render continuously, but only when `RENDER`
is called, or when an expose event is received.  You may do the
following to enable idle rendering:

```lisp
(setf (idle-render WINDOW) t)
```

You may do this at any time, and also set it to `NIL` to disable idle
rendering.

## Shaders

Often you want to compile and check shaders, maintain the programs,
and set various uniforms as parameters.  `sdl2.kit` provides a way to
do this simply:

```lisp
(defvar *my-programs*
  `((:program-name
     (:uniforms :v1 :v2 ...)
     (:shaders :vertex-shader ,shader-text
               :fragment-shader "..."
               ...))

    (:another-program ...)))
```

This is a simple structured list containing the specification for
creating a dictionary.  You may specify any legal combination of
shaders in the shaders section.  Reusing text between shaders is
relatively easy by using separate definitions and including them, as
per above.

To actually compile and use these, call the following; it will attempt
to compile and link all the specified programs, reporting any errors
along the way to `*error-output*`:

```lisp
(compile-shader-dictionary *my-programs*)
  ;; => DICTIONARY
```

This **requires** a valid GL context, will only work when it is
otherwise legal to make GL calls.  As well, the returned
`SHADER-DICTIONARY` object is only valid in the GL context in which it
was compiled.  It will **not** work in others.

Once you have this object, you may do interesting things with it:

```lisp
(sdl2.kit:use-program DICTIONARY :name)
(sdl2.kit:uniformi DICTIONARY :v1 0)
(sdl2.kit:uniformf DICTIONARY :v2 x y)

;; etc
```

Note these are different functions than the `cl-opengl` variety; they
take the *dictionary* object, as well as symbolic names, rather than
IDs.
