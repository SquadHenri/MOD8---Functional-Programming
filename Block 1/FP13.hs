data Sex = Male | Female
data Person = Person {
    name :: String,
    age :: Int,
    sex :: Sex,
    place_of_residence :: String
}


person1 = Person {name="Mark", age=51, 
sex=Male, place_of_residence="Enschede"}

person2 = Person {name="Carl", age=19, 
sex=Male, place_of_residence="Hengelo"}

person3 = Person {name="Thyra", age=34, 
sex=Female, place_of_residence="Vogelenzang"}

person4 = Person {name="Joris", age=99, 
sex=Male, place_of_residence="Nachtuil"}

person5 = Person {name="Maik", age=39, 
sex=Male, place_of_residence="Amsterdam"}

person6 = Person {name="Merol", age=31, 
sex=Female, place_of_residence="Maaskant"}

person7 = Person {name="Kech", age=63, 
sex=Female, place_of_residence="Maaskantje"}

person8 = Person {name="Messi", age=32, 
sex=Male, place_of_residence="Maaskant"}

db = [person1, person2, person3, person4, person5, person6, person7, person8]

addNtoAge :: Int -> [Person] -> [Person]
addNtoAge _ [] = []
addNtoAge n (person:xs) = person {age = age + n} : addNtoAge n xs
