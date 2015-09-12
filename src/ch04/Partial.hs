import Data.List (isInfixOf)

isInAny2 needle haystack = any(\s -> needle `isInfixOf` s) haystack

isInAny3 needle haystack = any (needle `isInfixOf`) haystack

