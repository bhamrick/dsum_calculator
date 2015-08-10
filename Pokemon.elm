module Pokemon where

import Dict exposing (Dict)

type alias Species =
    { indexNumber : Int
    , pokedexNumber : Int
    , name : String
    }

speciesByIndex : Dict Int Species
speciesByIndex =
    speciesList
        |> List.map (\species -> (species.indexNumber, species))
        |> Dict.fromList

speciesByPokedex : Dict Int Species
speciesByPokedex =
    speciesList
        |> List.map (\species -> (species.pokedexNumber, species))
        |> Dict.fromList

speciesByName : Dict String Species
speciesByName =
    speciesList
        |> List.map (\species -> (species.name, species))
        |> Dict.fromList

noSpecies : Species
noSpecies = { indexNumber = 0, pokedexNumber = 0, name = "" }

speciesList : List Species
speciesList =
    [ { indexNumber = 1, pokedexNumber = 112, name = "Rhydon" }
    , { indexNumber = 2, pokedexNumber = 115, name = "Kangaskhan" }
    , { indexNumber = 3, pokedexNumber = 32, name = "Nidoran M" }
    , { indexNumber = 4, pokedexNumber = 35, name = "Clefairy" }
    , { indexNumber = 5, pokedexNumber = 21, name = "Spearow" }
    , { indexNumber = 6, pokedexNumber = 100, name = "Voltorb" }
    , { indexNumber = 7, pokedexNumber = 34, name = "Nidoking" }
    , { indexNumber = 8, pokedexNumber = 80, name = "Slowbro" }
    , { indexNumber = 9, pokedexNumber = 2, name = "Ivysaur" }
    , { indexNumber = 10, pokedexNumber = 103, name = "Exeggutor" }
    , { indexNumber = 11, pokedexNumber = 108, name = "Lickitung" }
    , { indexNumber = 12, pokedexNumber = 102, name = "Exeggcute" }
    , { indexNumber = 13, pokedexNumber = 88, name = "Grimer" }
    , { indexNumber = 14, pokedexNumber = 94, name = "Gengar" }
    , { indexNumber = 15, pokedexNumber = 29, name = "Nidoran F" }
    , { indexNumber = 16, pokedexNumber = 31, name = "Nidoqueen" }
    , { indexNumber = 17, pokedexNumber = 104, name = "Cubone" }
    , { indexNumber = 18, pokedexNumber = 111, name = "Rhyhorn" }
    , { indexNumber = 19, pokedexNumber = 131, name = "Lapras" }
    , { indexNumber = 20, pokedexNumber = 59, name = "Arcanine" }
    , { indexNumber = 21, pokedexNumber = 151, name = "Mew" }
    , { indexNumber = 22, pokedexNumber = 130, name = "Gyarados" }
    , { indexNumber = 23, pokedexNumber = 90, name = "Shellder" }
    , { indexNumber = 24, pokedexNumber = 72, name = "Tentacool" }
    , { indexNumber = 25, pokedexNumber = 92, name = "Gastly" }
    , { indexNumber = 26, pokedexNumber = 123, name = "Scyther" }
    , { indexNumber = 27, pokedexNumber = 120, name = "Staryu" }
    , { indexNumber = 28, pokedexNumber = 9, name = "Blastoise" }
    , { indexNumber = 29, pokedexNumber = 127, name = "Pinsir" }
    , { indexNumber = 30, pokedexNumber = 114, name = "Tangela" }
    , { indexNumber = 33, pokedexNumber = 58, name = "Growlithe" }
    , { indexNumber = 34, pokedexNumber = 95, name = "Onix" }
    , { indexNumber = 35, pokedexNumber = 22, name = "Fearow" }
    , { indexNumber = 36, pokedexNumber = 16, name = "Pidgey" }
    , { indexNumber = 37, pokedexNumber = 79, name = "Slowpoke" }
    , { indexNumber = 38, pokedexNumber = 64, name = "Kadabra" }
    , { indexNumber = 39, pokedexNumber = 75, name = "Graveler" }
    , { indexNumber = 40, pokedexNumber = 113, name = "Chansey" }
    , { indexNumber = 41, pokedexNumber = 67, name = "Machoke" }
    , { indexNumber = 42, pokedexNumber = 122, name = "Mr. Mime" }
    , { indexNumber = 43, pokedexNumber = 106, name = "Hitmonlee" }
    , { indexNumber = 44, pokedexNumber = 107, name = "Hitmonchan" }
    , { indexNumber = 45, pokedexNumber = 24, name = "Arbok" }
    , { indexNumber = 46, pokedexNumber = 47, name = "Parasect" }
    , { indexNumber = 47, pokedexNumber = 54, name = "Psyduck" }
    , { indexNumber = 48, pokedexNumber = 96, name = "Drowzee" }
    , { indexNumber = 49, pokedexNumber = 76, name = "Golem" }
    , { indexNumber = 51, pokedexNumber = 126, name = "Magmar" }
    , { indexNumber = 53, pokedexNumber = 125, name = "Electabuzz" }
    , { indexNumber = 54, pokedexNumber = 82, name = "Magneton" }
    , { indexNumber = 55, pokedexNumber = 109, name = "Koffing" }
    , { indexNumber = 57, pokedexNumber = 56, name = "Mankey" }
    , { indexNumber = 58, pokedexNumber = 86, name = "Seel" }
    , { indexNumber = 59, pokedexNumber = 50, name = "Diglett" }
    , { indexNumber = 60, pokedexNumber = 128, name = "Tauros" }
    , { indexNumber = 64, pokedexNumber = 83, name = "Farfetch'd" }
    , { indexNumber = 65, pokedexNumber = 48, name = "Venonat" }
    , { indexNumber = 66, pokedexNumber = 149, name = "Dragonite" }
    , { indexNumber = 70, pokedexNumber = 84, name = "Doduo" }
    , { indexNumber = 71, pokedexNumber = 60, name = "Poliwag" }
    , { indexNumber = 72, pokedexNumber = 124, name = "Jynx" }
    , { indexNumber = 73, pokedexNumber = 146, name = "Moltres" }
    , { indexNumber = 74, pokedexNumber = 144, name = "Articuno" }
    , { indexNumber = 75, pokedexNumber = 145, name = "Zapdos" }
    , { indexNumber = 76, pokedexNumber = 132, name = "Ditto" }
    , { indexNumber = 77, pokedexNumber = 52, name = "Meowth" }
    , { indexNumber = 78, pokedexNumber = 98, name = "Krabby" }
    , { indexNumber = 82, pokedexNumber = 37, name = "Vulpix" }
    , { indexNumber = 83, pokedexNumber = 38, name = "Ninetales" }
    , { indexNumber = 84, pokedexNumber = 25, name = "Pikachu" }
    , { indexNumber = 85, pokedexNumber = 26, name = "Raichu" }
    , { indexNumber = 88, pokedexNumber = 147, name = "Dratini" }
    , { indexNumber = 89, pokedexNumber = 148, name = "Dragonair" }
    , { indexNumber = 90, pokedexNumber = 140, name = "Kabuto" }
    , { indexNumber = 91, pokedexNumber = 141, name = "Kabutops" }
    , { indexNumber = 92, pokedexNumber = 116, name = "Horsea" }
    , { indexNumber = 93, pokedexNumber = 117, name = "Seadra" }
    , { indexNumber = 96, pokedexNumber = 27, name = "Sandshrew" }
    , { indexNumber = 97, pokedexNumber = 28, name = "Sandslash" }
    , { indexNumber = 98, pokedexNumber = 138, name = "Omanyte" }
    , { indexNumber = 99, pokedexNumber = 139, name = "Omastar" }
    , { indexNumber = 100, pokedexNumber = 39, name = "Jigglypuff" }
    , { indexNumber = 101, pokedexNumber = 40, name = "Wigglytuff" }
    , { indexNumber = 102, pokedexNumber = 133, name = "Eevee" }
    , { indexNumber = 103, pokedexNumber = 136, name = "Flareon" }
    , { indexNumber = 104, pokedexNumber = 135, name = "Jolteon" }
    , { indexNumber = 105, pokedexNumber = 134, name = "Vaporeon" }
    , { indexNumber = 106, pokedexNumber = 66, name = "Machop" }
    , { indexNumber = 107, pokedexNumber = 41, name = "Zubat" }
    , { indexNumber = 108, pokedexNumber = 23, name = "Ekans" }
    , { indexNumber = 109, pokedexNumber = 46, name = "Paras" }
    , { indexNumber = 110, pokedexNumber = 61, name = "Poliwhirl" }
    , { indexNumber = 111, pokedexNumber = 62, name = "Poliwrath" }
    , { indexNumber = 112, pokedexNumber = 13, name = "Weedle" }
    , { indexNumber = 113, pokedexNumber = 14, name = "Kakuna" }
    , { indexNumber = 114, pokedexNumber = 15, name = "Beedrill" }
    , { indexNumber = 116, pokedexNumber = 85, name = "Dodrio" }
    , { indexNumber = 117, pokedexNumber = 57, name = "Primeape" }
    , { indexNumber = 118, pokedexNumber = 51, name = "Dugtrio" }
    , { indexNumber = 119, pokedexNumber = 49, name = "Venomoth" }
    , { indexNumber = 120, pokedexNumber = 87, name = "Dewgong" }
    , { indexNumber = 123, pokedexNumber = 10, name = "Caterpie" }
    , { indexNumber = 124, pokedexNumber = 11, name = "Metapod" }
    , { indexNumber = 125, pokedexNumber = 12, name = "Butterfree" }
    , { indexNumber = 126, pokedexNumber = 68, name = "Machamp" }
    , { indexNumber = 128, pokedexNumber = 55, name = "Golduck" }
    , { indexNumber = 129, pokedexNumber = 97, name = "Hypno" }
    , { indexNumber = 130, pokedexNumber = 42, name = "Golbat" }
    , { indexNumber = 131, pokedexNumber = 150, name = "Mewtwo" }
    , { indexNumber = 132, pokedexNumber = 143, name = "Snorlax" }
    , { indexNumber = 133, pokedexNumber = 129, name = "Magikarp" }
    , { indexNumber = 136, pokedexNumber = 89, name = "Muk" }
    , { indexNumber = 138, pokedexNumber = 99, name = "Kingler" }
    , { indexNumber = 139, pokedexNumber = 91, name = "Cloyster" }
    , { indexNumber = 141, pokedexNumber = 101, name = "Electrode" }
    , { indexNumber = 142, pokedexNumber = 36, name = "Clefable" }
    , { indexNumber = 143, pokedexNumber = 110, name = "Weezing" }
    , { indexNumber = 144, pokedexNumber = 53, name = "Persian" }
    , { indexNumber = 145, pokedexNumber = 105, name = "Marowak" }
    , { indexNumber = 147, pokedexNumber = 93, name = "Haunter" }
    , { indexNumber = 148, pokedexNumber = 63, name = "Abra" }
    , { indexNumber = 149, pokedexNumber = 65, name = "Alakazam" }
    , { indexNumber = 150, pokedexNumber = 17, name = "Pidgeotto" }
    , { indexNumber = 151, pokedexNumber = 18, name = "Pidgeot" }
    , { indexNumber = 152, pokedexNumber = 121, name = "Starmie" }
    , { indexNumber = 153, pokedexNumber = 1, name = "Bulbasaur" }
    , { indexNumber = 154, pokedexNumber = 3, name = "Venusaur" }
    , { indexNumber = 155, pokedexNumber = 73, name = "Tentacruel" }
    , { indexNumber = 157, pokedexNumber = 118, name = "Goldeen" }
    , { indexNumber = 158, pokedexNumber = 119, name = "Seaking" }
    , { indexNumber = 163, pokedexNumber = 77, name = "Ponyta" }
    , { indexNumber = 164, pokedexNumber = 78, name = "Rapidash" }
    , { indexNumber = 165, pokedexNumber = 19, name = "Rattata" }
    , { indexNumber = 166, pokedexNumber = 20, name = "Raticate" }
    , { indexNumber = 167, pokedexNumber = 33, name = "Nidorino" }
    , { indexNumber = 168, pokedexNumber = 30, name = "Nidorina" }
    , { indexNumber = 169, pokedexNumber = 74, name = "Geodude" }
    , { indexNumber = 170, pokedexNumber = 77, name = "Porygon" }
    , { indexNumber = 171, pokedexNumber = 142, name = "Aerodactyl" }
    , { indexNumber = 173, pokedexNumber = 81, name = "Magnemite" }
    , { indexNumber = 176, pokedexNumber = 4, name = "Charmander" }
    , { indexNumber = 177, pokedexNumber = 7, name = "Squirtle" }
    , { indexNumber = 178, pokedexNumber = 5, name = "Charmeleon" }
    , { indexNumber = 179, pokedexNumber = 8, name = "Wartortle" }
    , { indexNumber = 180, pokedexNumber = 6, name = "Charizard" }
    , { indexNumber = 185, pokedexNumber = 43, name = "Oddish" }
    , { indexNumber = 186, pokedexNumber = 44, name = "Gloom" }
    , { indexNumber = 187, pokedexNumber = 45, name = "Vileplume" }
    , { indexNumber = 188, pokedexNumber = 69, name = "Bellsprout" }
    , { indexNumber = 189, pokedexNumber = 70, name = "Weepinbell" }
    , { indexNumber = 190, pokedexNumber = 71, name = "Victreebel" }
    ]
