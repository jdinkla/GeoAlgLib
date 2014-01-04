\subsection{Funktionen zur Ausgabe in OpenGL (|OpenGL|)}
\module{OpenGL}

\begin{code}

module Main where

import Control.Monad
import Data.IORef
import System      ( ExitCode(..), exitWith )
import Graphics.UI.GLUT

data State = State {
   cam :: IORef (Int, Int, Int)
   }

makeState :: IO State
makeState = do
   ca <- newIORef (0,0,0)
   return $ State {
      cam = ca
      }

display :: State -> DisplayCallback
display state = do
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))
   clear [ ColorBuffer, DepthBuffer ]
   flush
   swapBuffers

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'q', Down) -> exitWith ExitSuccess
      (Char '\27', Down) -> exitWith ExitSuccess
      (_, _) -> return ()
   
main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    initialWindowSize $= Size 500 500
    createWindow progName
    state <- makeState
    displayCallback $= display state
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboard state)
    mainLoop

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   let vp = 0.8
       aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)

   matrixMode $= Projection
   loadIdentity
   frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10

   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))       


\end{code}
