module Main where

import Data.AddressBook
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.List (List, length)
import Prelude (Unit)


addressOne :: Address
addressOne = { street: "Av Campos Sales 767", city: "Natal", state: "RN" }

addressTwo :: Address
addressTwo = { street: "Av Campos Sales 120", city: "Natal", state: "RN" }

entryOne :: Entry
entryOne = {
  firstName: "Bernardo",
  lastName: "Gurgel",
  address: addressOne
}

entryTwo :: Entry
entryTwo = {
  firstName: "Bernardo",
  lastName: "Gurgel",
  address: addressTwo
}

addressBook :: List Entry
addressBook = insertEntry entryTwo (insertEntry entryOne emptyBook)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main =
  logShow(length (removeDuplicates(addressBook)))

