[[Category:Libraries]]
[[Category:Graphics]]

== About ==

This is a Haskell module for [http://www.glfw.org/ GLFW OpenGL framework]. It provides an alternative to GLUT for OpenGL based Haskell programs.

== Status ==

The library is being used by the [http://www.haskell.org/soe Haskell School of Expression (SOE)] code to render Graphics in a cross-platform manner. It currently interfaces with GLFW version 2.7.2, works on Windows, Linux (i386) and Mac OS X.

GLFW itself is well documented (see [http://www.glfw.org/  GLFW website]), and the Haskell module API is documented via Haddock.

Not all functions are fully tested, and there are still a
few GLFW C functions missing from the Haskell module, namely
the image loading functions. They are excluded because image
handling is a separate issue, and low level buffer manipulation
would obscure their use further. Texture loading from TGA
format is supported both from file and from memory (via a
string buffer)..

The Haskell module also provides basic text rendering while
GLFW doesn't. It comes from a free 8x16 font which is made
into a TGA texture, stored as a Haskell string in the file
GLFW.hs. Text rendering
is only possible with Alpha enabled. Again, see SOE.hs from
the SOE package for sample usage.

GLFW may not work well with GHC threads, forkIO or threadDelay.
So avoid them if you can.


== Download ==

Current version is [http://hackage.haskell.org/cgi-bin/hackage-scripts/package/GLFW GLFW-0.5.0.0]. It works with Cabal 1.10 or later. It compiles GLFW C source code as part of the building process, please report to the package maintainer if you have build problems.


== More information ==
* [http://hackage.haskell.org/packages/archive/GLFW/0.5.0.0/doc/html/Graphics-UI-GLFW.html The Haddock documentation]
* [http://www.glfw.org/ The GLFW site]


== Sample Program ==

To demonstrate the usage of GLFW for OpenGL based Haskell applications, here is a sample program that allows user to draw lines by holding the left mouse button and move the mouse.

\begin{code}
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)
\end{code}

Because the program needs to process user input, i.e., mouse button and movements, we'll use a continuation like structure for this purpose. The <hask>Action</hask> type represents an IO operation that returns the next <hask>Action</hask> to continue execution.

\begin{code}
data Action = Action (IO Action)
\end{code}

The main program is mostly book-keeping such as initializing OpenGL and GLFW, creating window, setting up viewport, etc.

\begin{code}
main = do
  -- invoke either active or passive drawing loop depending on command line argument
  args <- getArgs
  prog <- getProgName
  case args of
    ["active"]  -> putStrLn "Running in active mode" >> main' active 
    ["passive"] -> putStrLn "Running in passive mode" >> main' passive
    _ -> putStrLn $ "USAGE: " ++ prog ++ " [active|passive]"

main' run = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0

  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- run the main loop
  run lines
  -- finish up
  GLFW.closeWindow
  GLFW.terminate
\end{code}

There are usually two ways to structure the main loop of GLFW programs. One is by actively polling events before processing them. The screen buffer is usually redrawn every time before <hask>swapBuffers</hask> is called. This is the simplest main loop often seen in game applications, and may waste CPU cycles even when there is no visual update. Note that <hask>swapBuffers</hask> by default calls <hask>pollEvents</hask> implicitly, so there is no need to do a separate poll.

\begin{code}
-- we start with waitForPress action
active lines = loop waitForPress
  where 
 
    loop action = do
      -- draw the entire screen
      render lines
      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
            -- perform action
            Action action' <- action
            -- sleep for 1ms to yield CPU to other applications
            GLFW.sleep 0.001

            -- only continue when the window is not closed
            windowOpen <- getParam Opened
            unless (not windowOpen) $
              loop action' -- loop with next action

    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)
\end{code}

Another way to structure the main loop is to register event callbacks and use <hask>waitEvents</hask>. This way we don't have to put the program to sleep every 1ms because it'll not be using any CPU cycle when there is no event to handle.

One reminder in this approach is that <hask>swapBuffers</hask> must be handled with care, because it by default invokes <hask>pollEvents</hask>, which in turn invokes all callback functions. So if <hask>swapBuffers</hask> is used inside a callback, it'll create infinite loop and hang the program. To avoid it, we should disable the <hask>AutoPollEvent</hask> behavior using <hask>disableSpecial</hask>.

Another optimization we can do is to use a dirty marker to remember whether the screen really needs to be redrawn. This'll not only save CPU cycles but also speed up event processing to avoid piling up events in the event queue. Similar tricks can be done to optimize the active polling approach.

\begin{code}
passive lines = do
  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent

  -- keep track of whether ESC has been pressed
  quit <- newIORef False

  -- keep track of whether screen needs to be redrawn
  dirty <- newIORef True

  -- mark screen dirty in refresh callback which is often called
  -- when screen or part of screen comes into visibility.
  GLFW.windowRefreshCallback $= writeIORef dirty True

  -- use key callback to track whether ESC is pressed
  GLFW.keyCallback $= \k s -> 
     when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $ 
        writeIORef quit True
     
  -- Terminate the program if the window is closed
  GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

  -- by default start with waitForPress
  waitForPress dirty
  loop dirty quit
  where
 
    loop dirty quit = do
        GLFW.waitEvents
        -- redraw screen if dirty
        d <- readIORef dirty

        when d $ 
          render lines >> GLFW.swapBuffers

        writeIORef dirty False
        -- check if we need to quit the loop
        q <- readIORef quit
        unless q $
          loop dirty quit
 
    waitForPress dirty =
      do
        GLFW.mousePosCallback    $= \_ -> return ()

        GLFW.mouseButtonCallback $= \b s -> 
            when (b == GLFW.ButtonLeft && s == GLFW.Press) $
              do
                -- when left mouse button is pressed, add the point
                -- to lines and switch to waitForRelease action.
                (GL.Position x y) <- GL.get GLFW.mousePos
                modifyIORef lines (((x,y):) . ((x,y):))
                waitForRelease dirty
 
    waitForRelease dirty = 
      do 
        GLFW.mousePosCallback $= \(Position x y) ->
          do
            -- update the line with new ending position
            modifyIORef lines (((x,y):) . tail)
            -- mark screen dirty
            writeIORef dirty True

        GLFW.mouseButtonCallback $= \b s ->
            -- when left mouse button is released, switch back to
            -- waitForPress action.
            when (b == GLFW.ButtonLeft && s == GLFW.Release) $
              waitForPress dirty
\end{code}

The rest of the program goes below.

\begin{code}
render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
 

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3
\end{code}


== More examples and external links ==

A number of famous [http://nehe.gamedev.net/ NeHe OpenGL tutorials] have been translated into Haskell using GLFW and made available in the [http://hackage.haskell.org/package/nehe-tuts nehe-tuts package] by Jason Dagit. Code as well as executables are available via the package page.
