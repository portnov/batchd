
module Main where

import System.Environment
import System.Batchd
import System.Batchd.LibVirt

main :: IO ()
main = do
  [action, id] <- getArgs
  let libvirt = LibVirt True "qemu:///system"
  case action of
    "start" -> startHost libvirt id
    "stop"  -> stopHost libvirt id
    _ -> fail $ "unknown action"

