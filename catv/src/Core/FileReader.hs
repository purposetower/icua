{-# LANGUAGE DuplicateRecordFields #-}

module Core.FileReader (getFileContent, ReadAmount(..), ReadContent(..), getReadContent,
    PieceTable, PieceTableItem(..)) where

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)


data ReadAmount = FullRead | MaxRead { maxReadAmount :: Integer }

data ReadContent = FullReadContent { content :: String } | MaxReadContent { content :: String }

type PieceTable = [PieceTableItem]

data PieceTableItem = PieceTableItem { startPosition :: Integer, content :: String }


getFileContent :: Handle -> Integer -> PieceTable -> ReadAmount -> IO ReadContent
getFileContent handle handlePosition pieceTable readAmount = do
    hSeek handle AbsoluteSeek handlePosition
    case readAmount of
        FullRead -> do
            content <- getFileContentFullReadAcc handle handlePosition pieceTable
            return $ FullReadContent content

        MaxRead maxReadAmount -> do
            (content, fullRead) <- getFileContentMaxReadAcc handle handlePosition pieceTable maxReadAmount
            return $ if fullRead then FullReadContent content else MaxReadContent content


getFileContentFullReadAcc :: Handle -> Integer -> PieceTable -> IO String
getFileContentFullReadAcc handle handlePosition pieceTable = do
    isEOF <- hIsEOF handle
    if isEOF then
        return ""
    else
        do
            nextHandleChar <- hGetChar handle
            let nextChar = case getPieceTableContent pieceTable handlePosition of
                    Just x -> x
                    Nothing -> nextHandleChar

            -- we need unsafeInterleaveIO otherwise whole file is read!
            deferredContent <- unsafeInterleaveIO $
                getFileContentFullReadAcc handle (handlePosition + 1) pieceTable
            return $ nextChar : deferredContent


getFileContentMaxReadAcc :: Handle -> Integer -> PieceTable -> Integer -> IO (String, Bool)
getFileContentMaxReadAcc handle handlePosition pieceTable maxReadAmount = do
    if maxReadAmount == 0 then
        return ("", False)
    else
        do
            isEOF <- hIsEOF handle
            if isEOF then
                return ("", True)
            else
                do
                    nextHandleChar <- hGetChar handle
                    let nextChar = case getPieceTableContent pieceTable handlePosition of
                            Just x -> x
                            Nothing -> nextHandleChar

                    -- we need unsafeInterleaveIO otherwise whole file is read!
                    (deferredContent, deferredFullRead) <- unsafeInterleaveIO $
                        getFileContentMaxReadAcc handle (handlePosition + 1) pieceTable (maxReadAmount - 1)
                    return (nextChar : deferredContent, deferredFullRead)


getReadContent :: ReadContent -> String
getReadContent (FullReadContent content) = content

getReadContent (MaxReadContent content) = content


getPieceTableContent :: PieceTable -> Integer -> Maybe Char
getPieceTableContent [] _ = Nothing

getPieceTableContent ((PieceTableItem startPosition content):xs) handlePosition =
    if handlePosition >= startPosition && handlePosition < (startPosition + (toInteger $ length content))
        then Just $ content !! (fromIntegral (handlePosition - startPosition))
        else getPieceTableContent xs handlePosition
