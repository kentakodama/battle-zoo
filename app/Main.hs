module Main where

import System.IO (hFlush, stdout)
import Data.Char (isDigit)

type Name = String 
type Age = Int 
type Species = String 
type Genus = String 

type Fly = Int
type Swim = Int 
type Claw = Int 
type Bite = Int 
type Cleverness = Int 

data Powers = Powers { 
    fly :: Fly, 
    swim :: Swim, 
    claw :: Claw, 
    bite :: Bite, 
    cleverness :: Cleverness  
} deriving Show

data Animal 
    = Animal { 
        aName :: Name, 
        aAge :: Age, 
        aSpecies :: Species, 
        aGenus :: Genus, 
        aPowers :: Powers  
    }
    | UnknownAnimal
    deriving Show

extractAnimal :: Maybe Animal -> Animal
extractAnimal (Just a) = a
extractAnimal Nothing = UnknownAnimal

getAnimal :: [Animal] -> Name -> Animal 
getAnimal [] _ = extractAnimal Nothing  
getAnimal (x : xs) name | (aName x) == name = x 
                        | otherwise         = getAnimal xs name

setPowers :: Int -> Int -> Int -> Int -> Int -> Powers  
setPowers fly swim claw bite cleverness = Powers fly swim claw bite cleverness

updateAnimal :: Animal -> String -> String -> Animal 
updateAnimal animal "name" value = animal { aName = value }
updateAnimal animal "age" value = animal { aAge = (read value) }
updateAnimal animal "species" value = animal { aSpecies = value }
updateAnimal animal "genus" value = animal { aGenus = value }


updatePower :: Powers -> String -> Int -> Powers
updatePower powers "fly" powerLevel = powers { fly = powerLevel }
updatePower powers "swim" powerLevel = powers { swim = powerLevel }    
updatePower powers "claw" powerLevel = powers { claw = powerLevel }    
updatePower powers "bite" powerLevel = powers { bite = powerLevel }    
updatePower powers "cleverness" powerLevel = powers { cleverness = powerLevel }    

addAnimal :: [Animal] -> Animal -> [Animal]
addAnimal animals newAnimal = newAnimal : animals

deleteAnimal :: [Animal] -> Name -> [Animal]
deleteAnimal [] _ = []
deleteAnimal (x : xs) name | (aName x) == name = deleteAnimal xs name 
                            | otherwise = x : (deleteAnimal xs name) 

updateAnimalList :: [Animal] -> Animal -> [Animal]
updateAnimalList [] _ = []
updateAnimalList (x : xs) newAnimal 
    | (aName x) == aName newAnimal = newAnimal : (updateAnimalList xs newAnimal)
    | otherwise                    = x : (updateAnimalList xs newAnimal)

battleAnimals :: Animal -> Animal -> (Animal, Animal)
battleAnimals animal1 animal2 = if (total1 >= total2) then (animal1, animal2)
                                else (animal1, animal2)    
    where 
        total1 = getTotalPower animal1 
        total2 = getTotalPower animal2 
  
getTotalPower :: Animal -> Int 
getTotalPower animal = totalPower where 
    powers = aPowers animal
    totalPower = (fly powers) + (swim powers) + (bite powers) + (claw powers) + (cleverness powers)


adjustPowerLevels :: (Animal, Animal) -> (Animal, Animal)
adjustPowerLevels (winner, loser) = (newWinner, newLoser) where
    newWinner = addPower winner
    newLoser  = subtractPower loser

addPower :: Animal -> Animal
addPower animal = updatedAnimal where
    powers = aPowers animal
    currentFlyPower = fly powers
    updatedPowers = updatePower powers "fly" (currentFlyPower + 2) 
    updatedAnimal = animal { aPowers = updatedPowers }

subtractPower :: Animal -> Animal
subtractPower animal = updatedAnimal where
    powers = aPowers animal
    currentFlyPower = fly powers
    updatedPowers = updatePower powers "fly" (currentFlyPower - 2) 
    updatedAnimal = animal { aPowers = updatedPowers }

isTooWeak :: Animal -> Bool 
isTooWeak animal = if (getTotalPower animal) < 10 then True else False 

updateListWithLoser :: [Animal] -> Animal -> [Animal]
updateListWithLoser animals loser | isTooWeak loser = deleteAnimal animals (aName loser)
                                  | otherwise       = updateAnimalList animals loser  

replaceAnimalInList :: [Animal] -> Animal -> [Animal]
replaceAnimalInList [] _ = []
replaceAnimalInList (x : xs) animal 
    | (aName x) == (aName animal)  = animal : replaceAnimalInList xs animal 
    | otherwise = x : replaceAnimalInList xs animal 

displayAllAnimals :: [Animal] -> String 
displayAllAnimals [] = replicate 58 '='
displayAllAnimals (animal : others) =
        showAnimal animal
        ++ "\n"
        ++ displayAllAnimals others
        

showAnimal :: Animal -> String 
-- showAnimal Nothing = replicate 58 '='
showAnimal animal =
        "\nName: " 
        ++ aName animal
        ++ "\nSpecies: "
        ++ aSpecies animal
        ++ "\nGenus: "
        ++ aGenus animal
        ++ "\n"
        ++ "\nAge: "
        ++ show (aAge animal)
        ++ "\n"
        ++ "\nPowers: "
        ++ createPowersMessage (aPowers animal)
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
      
createPowersMessage :: Powers -> String 
createPowersMessage powers =
        "Fly Power: " 
        ++ show (fly powers)
        ++ "\nSwim Power: "
        ++ show (swim powers)
        ++ "\nBite Power: "
        ++ show (bite powers)
        ++ "\nClaw Power: "
        ++ show (claw powers)
        ++ "\nCleverness Power: "
        ++ show (cleverness powers)
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
    
startBattleZoo :: [Animal] -> IO ()
startBattleZoo animals = do
    putStrLn "\n\n\n=============== Welcome to Battle Zoo ==============="
    putStrLn $ replicate 58 '='
    -- putStrLn $ displayAllAnimals animals
    putStrLn "(1) Show All  (2) Add Animal  (3) Update Animal  (4) Delete Animal  (5) Battle Animals"
    choice <- prompt "Input choice: "
    case choice of
        "1" -> do
            putStrLn $ displayAllAnimals animals
            empty <- prompt "Press enter to go back"
            startBattleZoo animals
        "2" -> do
            name <- prompt "Enter name: "
            species <- prompt "Enter species: "
            genus <- prompt "Enter genus: "
            age <- prompt "Enter age: "
            -- putStrLn "You're almost done. Give " + name + " powers!"
            flyingLevel <- prompt "Enter flying Power: "
            swimmingLevel <- prompt "Enter swimming Power: "
            bitingLevel <- prompt "Enter biting Power: "
            clawingLevel <- prompt "Enter clawing Power: "
            clevernessLevel <- prompt "Enter cleverness Power: "
            let powers = setPowers(read flyingLevel)(read swimmingLevel)(read bitingLevel)(read clawingLevel)(read clevernessLevel)
            let newAnimal = (Animal name (read age) species genus powers)
            let newList = addAnimal animals newAnimal
            putStrLn $ replicate 58 '='
            putStr "Congratulations! You create a new animal:"    
            putStrLn $ showAnimal newAnimal 
            empty <- prompt "Press enter to go back"
            startBattleZoo newList -- revert to newList
        "3" -> do
            name <- prompt "Enter name of animal to update: "
            let potentialAnimal = getAnimal animals name
            property <- prompt "What information do you want to update? \nChoose name, species, genus, or age: \n"
            value <- prompt $ "What do you want the new " ++ property ++ " to be?\n"
            let updatedAnimal = updateAnimal potentialAnimal property value 
            let newList = updateAnimalList animals updatedAnimal 
            putStr $ "Congratulations! You updated " ++ name  
            putStrLn $ replicate 58 '='
            putStrLn $ showAnimal updatedAnimal
            empty <- prompt "Press enter to go back"
            startBattleZoo newList
        "4" -> do
            name <- prompt "Enter name of animal to delete: "
            let newList = deleteAnimal animals name 
            putStr $ "You have deleted: " ++ name    
            empty <- prompt "\nPress enter to go back"
            startBattleZoo newList
        "5" -> do
            putStr "Looks like you want to battle animals!\nYou must enter two names of animals\n"
            name1 <- prompt "Enter name of animal one: "
            name2 <- prompt "Enter name of animal two: "
            putStr $ "You have chosen: " ++ name1 ++ " and " ++ name2 ++ "!"
            empty <- prompt "\nPress enter to begin the battle!"
            let animal1 = getAnimal animals name1
            let animal2 = getAnimal animals name2
            let (winner, loser) = battleAnimals animal1 animal2
            putStrLn $ "The winner is: " ++ (aName winner) ++ "!\n"
            putStrLn $ "The loser is: " ++ (aName loser) ++ "!\n"
            empty <- prompt "\nWinners get more power and losers must suffer the consequences \nPress enter to update their powers!"
            let (updatedWinner, updatedLoser) = adjustPowerLevels (winner, loser)
            let listWithWinner = updateAnimalList animals updatedWinner
            let finalList = updateListWithLoser listWithWinner loser
            putStrLn $ displayAllAnimals animals
            startBattleZoo finalList
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            startBattleZoo animals


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

powers1 = Powers 0 3 9 9 7
powers2 = Powers 0 2 2 2 3
animal1 = Animal "kenta" 23 "lion" "mammal" powers1
animal2 = Animal "bob" 17 "shark" "fish" powers1
animal3 = Animal "bob" 100 "shark" "fish" powers1
animal4 = Animal "weak" 100 "shark" "fish" powers2
animalList = [animal1, animal2, animal3, animal4]

main :: IO ()
main = do
    putStrLn "Welcome to: BATTLE ZOO"
    startBattleZoo animalList 
