module Main where
import SimpleJSON
import PutJSON

main = putJValue (JObject [("foo", JNumber 1), ("bar", JArray [JBool False, JNumber 3.8])])

