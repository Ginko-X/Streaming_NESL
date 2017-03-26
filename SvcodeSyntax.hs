module SvcodeSyntax where

import SneslSyntax

type SId = String  -- stream id

data Instr = Ctrl   -- streaming operations/transducers
           | ToFlags SId
           | Rep SId AVal
           | Iotas SId
           | Usum SId
           | Map_add SId SId
           | Pack SId SId
           deriving Show

data SDef = SDef SId Instr deriving Show 

data SSym = SSym [SDef] SId deriving Show  -- SId is the returned stream

data SvVal = SSVal [AVal]  -- sequence
           | SPVal SvVal SvVal -- pair


instance Show SvVal where
    show (SSVal s) = "<" ++ showseq s ++ ">"    
    show (SPVal f s) = "(" ++ show f ++"," ++ show s ++ ")"

showseq [] = ""
showseq [x] = show x
showseq (x:xs) = show x ++ ", " ++ showseq xs
