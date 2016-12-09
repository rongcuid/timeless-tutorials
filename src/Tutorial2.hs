{-# LANGUAGE Arrows #-}
module Tutorial2 (
  main
  -- * Tutorial 2 -- Text Adventure

  -- ** Introduction
  -- $intro
  ) where

-- $intro
-- Since my first motivation to start FRP at all is game programming, let's
-- try to make a tiny adventure game out of Timeless. No modularity is enforced,
-- and game structure may be messy, because this is basically the second complete
-- game I have ever written, the first being Sphase Invader in previous versions
-- of tutorial.
--
-- The game flow is extremely simple. You wake up finding yourself inside a room.
-- When you try to open the door, though... Well, it is not locked. Game clear!
-- So let's see what happens in the code.

import Prelude hiding ((.))

import FRP.Timeless
import Control.Concurrent
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Set as Set
import Data.Set(Set)

sMVarSource mvar = sourceS $ tryTakeMVar mvar

initPrint :: IO (MVar String)
initPrint = do
  mvar <- newEmptyMVar
  forkIO $ loop mvar
  return mvar
    where
      loop mvar = do
        s <- getLine
        putMVar mvar s
        loop mvar

sPutStrSink = sinkS $ \s -> putStr s >> hFlush stdout

data GameState = G_Running
               | G_Cleared
               deriving (Eq)

data Verb = V_Describe
          | V_Open
          deriving (Eq, Ord)

instance Show Verb where
  show V_Describe = "describe"
  show V_Open = "open"

readV "describe" = Just V_Describe
readV "open" = Just V_Open
readV _ = Nothing

data Object = OBJ_Room
            | OBJ_Door
            deriving (Eq, Ord)

instance Show Object where
  show OBJ_Room = "room"
  show OBJ_Door = "door"

readO "room" = Just OBJ_Room
readO "door" = Just OBJ_Door
readO _ = Nothing

data Action = ACT_Describe Object
            | ACT_Open_Door

type KnowledgeO = Set Object

type KnowledgeV = Set Verb

describe (ACT_Describe OBJ_Room) = "The room only has a door."
describe (ACT_Describe OBJ_Door) = "The door is not locked."
describe (ACT_Open_Door) = "You opened the door. Game is cleared!"
describe _ = "What do you mean?"

createAction (V_Describe,o) = Just $ ACT_Describe o
createAction (V_Open,OBJ_Door) = Just ACT_Open_Door
createAction _ = Nothing


validVO :: (Verb,Object) -> (KnowledgeV, KnowledgeO) -> Maybe (Verb,Object)
validVO (v, o) (kvs, kos) = if (v `Set.member` kvs) && (o `Set.member` kos)
                                  then Just (v, o)
                                  else Nothing

initialKO = Set.fromList [OBJ_Room]
updateKO ((ACT_Describe OBJ_Room), ks) = Set.insert OBJ_Door ks
updateKO (_, ks) = ks

initialKV = Set.fromList [V_Describe]
updateKV ((ACT_Describe OBJ_Room), ks) = Set.insert V_Open ks
updateKV (_, ks) = ks

updateGS (ACT_Open_Door, gs) = G_Cleared
updateGS (_,gs) = gs

readVO :: (String, String) -> Maybe (Verb, Object)
readVO = runKleisli (Kleisli readV *** Kleisli readO)

--display (validAction, gameState) =
display (anyAction, (gameState, kvs, kos))
  = let
  vList = intercalate " " $ (show <$> Set.toList kvs)
  oList = intercalate " " $ (show <$> Set.toList kos)
  desc = if (gameState == G_Running)
            then describe anyAction
            else "Game Cleared!"
  descSec = "\n======\n" ++ desc ++ "\n"
  helpSec = "------\nAvailable Verbs: " ++ vList ++ " Available Objects: " ++ oList ++ "\n"
  promptSec = "------\n>>> "
  in descSec ++ helpSec ++ promptSec

sInput :: MVar String -> StreamSource (Verb, Object)
sInput mvar = proc () -> do
  sString <- sMVarSource mvar -< ()
  sVO <- (arrS $ (\(v:o:_)->(v,o)) . take 2 . splitOn " ") -< sString
  returnA <<< arr (>>=readVO) -< sVO

--sGameLogic :: Stream (Verb, Object) (Action, GameState)
sGameLogic = loop $ proc (sVO', (cKV, cKO)) -> do
  sInitVO <- onceS (V_Describe, OBJ_Room) -< ()
  sVO <- mergeSP -< (sInitVO, sVO')
  sAnyAction <- arr (>>=createAction) -< sVO
  sAction <- arr (>>=createAction) <<< filterSM <<< snapshot (uncurry validVO) -< (sVO, (cKV, cKO))
  cKV' <- state initialKV updateKV -< sAction
  cKO' <- state initialKO updateKO -< sAction
  cKV'' <- delay initialKV -< cKV'
  cKO'' <- delay initialKO -< cKO'
  cGS <- state G_Running updateGS -< sAction
  returnA -< ((sAnyAction, cGS, cKV', cKO'), (cKV'', cKO''))

sOutput = proc input@(sAnyAction, cGameState, cKV, cKO) -> do
  sDisp <- snapshot display -< (sAnyAction, (cGameState, cKV, cKO))
  sPutStrSink -< sDisp

box mvar = sInput mvar >>> sGameLogic >>> sOutput

main :: IO ()
main = do
  mvar <- initPrint
  runBox $ box mvar
