module GetLineRewrite (getLineRewrite) where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Bifunctor
import           System.IO
import           Control.Monad.State

type Zipper = (String, String)

updateZippers :: Char -> [Zipper] -> Either String [Zipper]
updateZippers c = merge <$> mapM f
  where
    f :: Zipper -> Either String [Zipper]
    f z@(xs, y:ys) | c /= y    = Right [([], foldl (flip (:)) ys (y:xs))]
                   | otherwise = if null ys
                                   then Left (reverse (y:xs))
                                   else Right ((y:xs, ys) : [z | null xs])

    merge :: [[Zipper]] -> [Zipper]
    merge = map head . group . sort . concat

type Z = StateT (String, [[Zipper]]) IO

push :: Char -> [Zipper] -> Z ()
push c zs = modify (bimap (c:) (zs:))

pop :: Z ()
pop = modify (bimap tail tail)

ansiErase :: Int -> IO ()
ansiErase n = do putStr "\ESC["
                 putStr (show n)
                 putStr "D\ESC[K"

getModes :: IO (BufferMode, BufferMode, Bool)
getModes = do bi <- hGetBuffering stdin
              bo <- hGetBuffering stdout
              eo <- hGetEcho      stdout
              pure (bi, bo, eo)

setModes :: (BufferMode, BufferMode, Bool) -> IO ()
setModes (bi, bo, eo) = do hSetBuffering stdin  bi
                           hSetBuffering stdout bo
                           hSetEcho      stdout eo

getLineRewrite :: [(String,String)] -> IO String
getLineRewrite xs | any (null . fst) xs = error "getLineRewrite called with a rule to rewrite the empty string"
getLineRewrite xs = do ms <- getModes
                       setModes (NoBuffering, NoBuffering, False)
                       x <- evalStateT run initialState
                       setModes ms
                       pure (reverse x)
  where
    initialState ::(String, [[Zipper]])
    initialState = ("", (pure . sort) (map (\(x,_) -> ([],x)) xs))

    m :: Map String String
    m = M.fromList xs

    run :: Z String
    run = do b <- liftIO getChar >>= handler
             if b
               then fst <$> get
               else run

    handler :: Char -> Z Bool
    handler '\n'   = do liftIO (putChar '\n')
                        pure True
    handler '\DEL' = do x <- fst <$> get
                        unless (null x)
                          (liftIO (ansiErase 1) >> pop)
                        pure False
    handler '\ESC' =    pure False
    handler c      = do zs <- head . snd <$> get
                        case updateZippers c zs of
                          Right zs' -> do liftIO (putChar c)
                                          push c zs'
                          Left  as  -> do let n       = length as - 1
                                          let Just bs = M.lookup as m
                                          when (n > 0) (liftIO (ansiErase n))
                                          replicateM_ n pop
                                          mapM_ handler bs
                        pure False
