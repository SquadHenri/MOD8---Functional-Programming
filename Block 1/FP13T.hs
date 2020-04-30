import Data.Char
import Data.List

data Sex_ = Male | Female deriving(Show, Eq, Ord)
type Name = String
type Age = Int
type Sex = Sex_
type PlaceOfResidence = String

type Person = [(Name, Age, Sex, PlaceOfResidence)]

db :: Person
db = 
    [
    ("Rowin", 22, Male, "Hoenderloo"), 
    ("Marieke", 21, Female, "Maaskantje"),
    ("Maik", 33, Male, "Dordrect"),
    ("Merol", 31, Female, "Blabla"),
    ("Alexander", 44, Male, "Oehoe"),
    ("Jan-Willem", 64, Male, "Gooi"),
    ("Thyra", 43, Female, "Vogeltjes"),
    ("Messi", 32, Male, "Barelona"),
    ("Martens", 35, Female, "Barcelona"),
    ("Ronaldo", 33, Male, "Turijn")]

getName :: Person -> Name
getName [(a, _, _ , _)] = a

getAge :: Person -> Age
getAge [(_, a, _, _)] = a

getSex :: Person -> Sex
getSex [(_, _, a, _)] = a

getPlaceOfResidence :: Person -> PlaceOfResidence
getPlaceOfResidence [(_, _, _, a)] = a

addAgeRecursion :: Age -> Person -> Person
addAgeRecursion _ [] = []
addAgeRecursion n ((name, age, sex, placeOfResidence):rest) = (name, newAge, sex, placeOfResidence) : addAgeRecursion n rest
    where newAge = age + n

addAgeListComprehension :: Age -> Person -> Person
addAgeListComprehension n people = [(a, b+n, c, d) | (a, b, c, d) <- people]

--addAgeHighOrder :: Age -> Person -> Person
--addAgeHighOrder x people = map (\(name, age, sex, placeOfResidence) -> (name,(+x) b,sex,placeOfResidence)) people

getBetweenAge :: Person -> Person
getBetweenAge [] = []
getBetweenAge ((name, age, sex, placeOfResidence):rest)
    | (age >= 30 && age <= 40) && sex == Female = (name, age, sex, placeOfResidence) : getBetweenAge rest
    | otherwise = getBetweenAge rest

getBetweenAgeList :: Person -> Person
getBetweenAgeList people = [(a, b, c, d) | (a, b, c, d) <- people, b <= 40 && b >= 30 && c == Female]

getBetweenAgeListHigh :: Person -> Person
getBetweenAgeListHigh person = filter (\(_,age,sex, _)-> age >= 30 && age <= 40 && sex == Female) db

getAgeByName :: Name -> Person -> Person
getAgeByName name people = [(a, b, c, d) | (a, b, c, d) <- people, map toLower a == map toLower name]

flipTuple :: Person -> [(Age, Name, Sex, PlaceOfResidence)]
flipTuple [] = []
flipTuple ((name, age, sex, placeOfResidence):rest) = (age, name, sex, placeOfResidence) : flipTuple rest

reflipTuple :: [(Age, Name, Sex, PlaceOfResidence)] -> Person
reflipTuple [] = []
reflipTuple ((age, name, sex, placeOfResidence):rest) = (name, age, sex, placeOfResidence) : reflipTuple rest

sortDb :: Person -> Person
sortDb people = reflipTuple (sort (flipTuple db))