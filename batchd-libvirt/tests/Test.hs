
module Main where

import System.Environment
import Batchd.Core
import Batchd.Ext.LibVirt

main :: IO ()
main = do
  [action, id] <- getArgs
  let libvirt = LibVirt True "qemu:///system"
  case action of
    "start" -> startHost libvirt id
    "stop"  -> stopHost libvirt id
    _ -> fail $ "unknown action"

