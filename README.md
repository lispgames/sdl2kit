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
* `close-window WINDOW`:  Close the window.  If you add a method to this, the window will not be closed unless you `CALL-NEXT-METHOD`.  This may be useful!

**NOTE:** `INITIALIZE-INSTANCE` requires you `CALL-NEXT-METHOD` first,
or simply define your method as `:AFTER`.  (This is due to method ordering being most-specific-first.)

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

Shader dictionary support in `sdl2kit` is **deprecated**.  This has
been moved to [glkit](https://github.com/lispgames/glkit).
