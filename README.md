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
* Integrating with binary generation for easy startup/exit

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

## Render Disable

When an unhandled error happens in `RENDER`, this could lead to
unpleasant effects: often a long string of errors until the window is
close, or the error is fixed, especially if `IDLE-RENDER` is enabled.

However, sdl2kit will automatically *disable* window rendering if such
an error occurs.  This will prevent further errors from occurring.
To re-enable or check the status of rendering, use the following:

* `(render-enabled WINDOW)`: Return the status of rendering
* `(setf (render-enabled WINDOW) BOOLEAN)`: Set the state of window rendering

Note that this only prevents *rendering*; events and other callbacks
will still be active.  This makes it easy to close a window if desired
before fixing the issue.

## Startup Functions and Making Binaries

While you can call `(kit.sdl2:start)` and `(make-instance 'my-window)`
manually, you should define a *startup function*.  This will ease a
number of things:

```lisp
(define-start-function run-my-game (&key (w 1280) (h 720))
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :stencil-size 8)
  (make-instance 'game-window :w w :h h))
```

This wraps the function in `SDL2:IN-MAIN-THREAD`, as well as pre- and
post-initialization.

This is convenient, but it's even more important for making binaries.
When you dump a lisp image, this makes sure the initialization and
startup of cl-sdl2 and sdl2kit are in the right order.

Typically when you dump a binary, you define a "toplevel function" to
be called.  It should look something like this:

```lisp
(save-my-lisp
 :toplevel-function
 (lambda () (sdl2:make-this-thread-main 'run-my-game)))
 ```

This will prevent additional threads from being created (unless your
game makes them itself), and everything should run and exit seamlessly
(but see below).  `(RUN-MY-GAME)` should also work normally in Sly or
SLIME.

## Implementation specifics

It's worth noting that in some lisp implementations, all SDL and OpenGL 
calls must be made from the main thread. The `examples/rotating-cube.lisp`
can be run as follows:

```lisp
(ql:quickload :sdl2kit-examples)

(sdl2.kit:define-start-function cube-test (&key (w 800) (h 500))
  (sdl2:gl-set-attr :stencil-size 8)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (make-instance 'kit.sdl2.test:cube-window :w w :h h))

(sdl2:make-this-thread-main #'cube-test)
```

## Exiting

Quitting sdl2kit can by done with `(kit.sdl2:quit)`.  This also quits
`cl-sdl2`, calling `(sdl2:quit)`.  Normally, you should not need to do
this during development, or if you're running things in Sly or SLIME.
However, if you're making a binary, you probably want your game to
exit!  In this case, calling `(kit.sdl2:quit)` during `CLOSE-WINDOW`
or some other event should be sufficient to exit, if you're using a
startup function as above.

Build integration is coming soon, so it's easy to tell whether you're
running "as a binary" or not.  Currently to do this, you will need to
set some flag yourself during the build process, which is also
available during normal runs.
