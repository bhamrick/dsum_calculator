module Pokemon where

import Dict exposing (Dict)

type alias Species =
    { indexNumber : Int
    , pokedexNumber : Int
    , name : String
    , cryDiff : Int
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
noSpecies =
    { indexNumber = 0
    , pokedexNumber = 0
    , name = ""
    , cryDiff = 0
    }

speciesList : List Species
speciesList =
    [ { indexNumber = 1
      , pokedexNumber = 112
      , name = "Rhydon"
      , cryDiff = 49
      }
    , { indexNumber = 2
      , pokedexNumber = 115
      , name = "Kangaskhan"
      , cryDiff = 41
      }
    , { indexNumber = 3
      , pokedexNumber = 32
      , name = "Nidoran M"
      , cryDiff = 16
      }
    , { indexNumber = 4
      , pokedexNumber = 35
      , name = "Clefairy"
      , cryDiff = 6
      }
    , { indexNumber = 5
      , pokedexNumber = 21
      , name = "Spearow"
      , cryDiff = 43
      }
    , { indexNumber = 6
      , pokedexNumber = 100
      , name = "Voltorb"
      , cryDiff = 57
      }
    , { indexNumber = 7
      , pokedexNumber = 34
      , name = "Nidoking"
      , cryDiff = 59
      }
    , { indexNumber = 8
      , pokedexNumber = 80
      , name = "Slowbro"
      , cryDiff = 28
      }
    , { indexNumber = 9
      , pokedexNumber = 2
      , name = "Ivysaur"
      , cryDiff = 33
      }
    , { indexNumber = 10
      , pokedexNumber = 103
      , name = "Exeggutor"
      , cryDiff = 78
      }
    , { indexNumber = 11
      , pokedexNumber = 108
      , name = "Lickitung"
      , cryDiff = 34
      }
    , { indexNumber = 12
      , pokedexNumber = 102
      , name = "Exeggcute"
      , cryDiff = 43
      }
    , { indexNumber = 13
      , pokedexNumber = 88
      , name = "Grimer"
      , cryDiff = 18
      }
    , { indexNumber = 14
      , pokedexNumber = 94
      , name = "Gengar"
      , cryDiff = 30
      }
    , { indexNumber = 15
      , pokedexNumber = 29
      , name = "Nidoran F"
      , cryDiff = 15
      }
    , { indexNumber = 16
      , pokedexNumber = 31
      , name = "Nidoqueen"
      , cryDiff = 41
      }
    , { indexNumber = 17
      , pokedexNumber = 104
      , name = "Cubone"
      , cryDiff = 26
      }
    , { indexNumber = 18
      , pokedexNumber = 111
      , name = "Rhyhorn"
      , cryDiff = 44
      }
    , { indexNumber = 19
      , pokedexNumber = 131
      , name = "Lapras"
      , cryDiff = 30
      }
    , { indexNumber = 20
      , pokedexNumber = 59
      , name = "Arcanine"
      , cryDiff = 36
      }
    , { indexNumber = 21
      , pokedexNumber = 151
      , name = "Mew"
      , cryDiff = 79
      }
    , { indexNumber = 22
      , pokedexNumber = 130
      , name = "Gyarados"
      , cryDiff = 44
      }
    , { indexNumber = 23
      , pokedexNumber = 90
      , name = "Shellder"
      , cryDiff = 30
      }
    , { indexNumber = 24
      , pokedexNumber = 72
      , name = "Tentacool"
      , cryDiff = 31
      }
    , { indexNumber = 25
      , pokedexNumber = 92
      , name = "Gastly"
      , cryDiff = 52
      }
    , { indexNumber = 26
      , pokedexNumber = 123
      , name = "Scyther"
      , cryDiff = 24
      }
    , { indexNumber = 27
      , pokedexNumber = 120
      , name = "Staryu"
      , cryDiff = 44
      }
    , { indexNumber = 28
      , pokedexNumber = 9
      , name = "Blastoise"
      , cryDiff = 43
      }
    , { indexNumber = 29
      , pokedexNumber = 127
      , name = "Pinsir"
      , cryDiff = 22
      }
    , { indexNumber = 30
      , pokedexNumber = 114
      , name = "Tangela"
      , cryDiff = 29
      }
    , { indexNumber = 33
      , pokedexNumber = 58
      , name = "Growlithe"
      , cryDiff = 26
      }
    , { indexNumber = 34
      , pokedexNumber = 95
      , name = "Onix"
      , cryDiff = 59
      }
    , { indexNumber = 35
      , pokedexNumber = 22
      , name = "Fearow"
      , cryDiff = 35
      }
    , { indexNumber = 36
      , pokedexNumber = 16
      , name = "Pidgey"
      , cryDiff = 2
      }
    , { indexNumber = 37
      , pokedexNumber = 79
      , name = "Slowpoke"
      , cryDiff = 9
      }
    , { indexNumber = 38
      , pokedexNumber = 64
      , name = "Kadabra"
      , cryDiff = 67
      }
    , { indexNumber = 39
      , pokedexNumber = 75
      , name = "Graveler"
      , cryDiff = 66
      }
    , { indexNumber = 40
      , pokedexNumber = 113
      , name = "Chansey"
      , cryDiff = 31
      }
    , { indexNumber = 41
      , pokedexNumber = 67
      , name = "Machoke"
      , cryDiff = 27
      }
    , { indexNumber = 42
      , pokedexNumber = 122
      , name = "Mr. Mime"
      , cryDiff = 39
      }
    , { indexNumber = 43
      , pokedexNumber = 106
      , name = "Hitmonlee"
      , cryDiff = 41
      }
    , { indexNumber = 44
      , pokedexNumber = 107
      , name = "Hitmonchan"
      , cryDiff = 43
      }
    , { indexNumber = 45
      , pokedexNumber = 24
      , name = "Arbok"
      , cryDiff = 41
      }
    , { indexNumber = 46
      , pokedexNumber = 47
      , name = "Parasect"
      , cryDiff = 81
      }
    , { indexNumber = 47
      , pokedexNumber = 54
      , name = "Psyduck"
      , cryDiff = 26
      }
    , { indexNumber = 48
      , pokedexNumber = 96
      , name = "Drowzee"
      , cryDiff = 70
      }
    , { indexNumber = 49
      , pokedexNumber = 76
      , name = "Golem"
      , cryDiff = 28
      }
    , { indexNumber = 51
      , pokedexNumber = 126
      , name = "Magmar"
      , cryDiff = 31
      }
    , { indexNumber = 53
      , pokedexNumber = 125
      , name = "Electabuzz"
      , cryDiff = 93
      }
    , { indexNumber = 54
      , pokedexNumber = 82
      , name = "Magneton"
      , cryDiff = 65
      }
    , { indexNumber = 55
      , pokedexNumber = 109
      , name = "Koffing"
      , cryDiff = 47
      }
    , { indexNumber = 57
      , pokedexNumber = 56
      , name = "Mankey"
      , cryDiff = 32
      }
    , { indexNumber = 58
      , pokedexNumber = 86
      , name = "Seel"
      , cryDiff = 39
      }
    , { indexNumber = 59
      , pokedexNumber = 50
      , name = "Diglett"
      , cryDiff = 42
      }
    , { indexNumber = 60
      , pokedexNumber = 128
      , name = "Tauros"
      , cryDiff = 38
      }
    , { indexNumber = 64
      , pokedexNumber = 83
      , name = "Farfetch'd"
      , cryDiff = 18
      }
    , { indexNumber = 65
      , pokedexNumber = 48
      , name = "Venonat"
      , cryDiff = 30
      }
    , { indexNumber = 66
      , pokedexNumber = 149
      , name = "Dragonite"
      , cryDiff = 44
      }
    , { indexNumber = 70
      , pokedexNumber = 84
      , name = "Doduo"
      , cryDiff = 43
      }
    , { indexNumber = 71
      , pokedexNumber = 60
      , name = "Poliwag"
      , cryDiff = 18
      }
    , { indexNumber = 72
      , pokedexNumber = 124
      , name = "Jynx"
      , cryDiff = 121
      }
    , { indexNumber = 73
      , pokedexNumber = 146
      , name = "Moltres"
      , cryDiff = 54
      }
    , { indexNumber = 74
      , pokedexNumber = 144
      , name = "Articuno"
      , cryDiff = 53
      }
    , { indexNumber = 75
      , pokedexNumber = 145
      , name = "Zapdos"
      , cryDiff = 31
      }
    , { indexNumber = 76
      , pokedexNumber = 132
      , name = "Ditto"
      , cryDiff = 20
      }
    , { indexNumber = 77
      , pokedexNumber = 52
      , name = "Meowth"
      , cryDiff = 8
      }
    , { indexNumber = 78
      , pokedexNumber = 98
      , name = "Krabby"
      , cryDiff = 56
      }
    , { indexNumber = 82
      , pokedexNumber = 37
      , name = "Vulpix"
      , cryDiff = 54
      }
    , { indexNumber = 83
      , pokedexNumber = 38
      , name = "Ninetales"
      , cryDiff = 56
      }
    , { indexNumber = 84
      , pokedexNumber = 25
      , name = "Pikachu"
      , cryDiff = 32
      }
    , { indexNumber = 85
      , pokedexNumber = 26
      , name = "Raichu"
      , cryDiff = 55
      }
    , { indexNumber = 88
      , pokedexNumber = 147
      , name = "Dratini"
      , cryDiff = 32
      }
    , { indexNumber = 89
      , pokedexNumber = 148
      , name = "Dragonair"
      , cryDiff = 33
      }
    , { indexNumber = 90
      , pokedexNumber = 140
      , name = "Kabuto"
      , cryDiff = 21
      }
    , { indexNumber = 91
      , pokedexNumber = 141
      , name = "Kabutops"
      , cryDiff = 26
      }
    , { indexNumber = 92
      , pokedexNumber = 116
      , name = "Horsea"
      , cryDiff = 7
      }
    , { indexNumber = 93
      , pokedexNumber = 117
      , name = "Seadra"
      , cryDiff = 6
      }
    , { indexNumber = 96
      , pokedexNumber = 27
      , name = "Sandshrew"
      , cryDiff = 16
      }
    , { indexNumber = 97
      , pokedexNumber = 28
      , name = "Sandslash"
      , cryDiff = 33
      }
    , { indexNumber = 98
      , pokedexNumber = 138
      , name = "Omanyte"
      , cryDiff = 24
      }
    , { indexNumber = 99
      , pokedexNumber = 139
      , name = "Omastar"
      , cryDiff = 26
      }
    , { indexNumber = 100
      , pokedexNumber = 39
      , name = "Jigglypuff"
      , cryDiff = 1
      }
    , { indexNumber = 101
      , pokedexNumber = 40
      , name = "Wigglytuff"
      , cryDiff = 8
      }
    , { indexNumber = 102
      , pokedexNumber = 133
      , name = "Eevee"
      , cryDiff = 32
      }
    , { indexNumber = 103
      , pokedexNumber = 136
      , name = "Flareon"
      , cryDiff = 31
      }
    , { indexNumber = 104
      , pokedexNumber = 135
      , name = "Jolteon"
      , cryDiff = 34
      }
    , { indexNumber = 105
      , pokedexNumber = 134
      , name = "Vaporeon"
      , cryDiff = 55
      }
    , { indexNumber = 106
      , pokedexNumber = 66
      , name = "Machop"
      , cryDiff = 26
      }
    , { indexNumber = 107
      , pokedexNumber = 41
      , name = "Zubat"
      , cryDiff = 41
      }
    , { indexNumber = 108
      , pokedexNumber = 23
      , name = "Ekans"
      , cryDiff = 39
      }
    , { indexNumber = 109
      , pokedexNumber = 46
      , name = "Paras"
      , cryDiff = 71
      }
    , { indexNumber = 110
      , pokedexNumber = 61
      , name = "Poliwhirl"
      , cryDiff = 6
      }
    , { indexNumber = 111
      , pokedexNumber = 62
      , name = "Poliwrath"
      , cryDiff = 19
      }
    , { indexNumber = 112
      , pokedexNumber = 13
      , name = "Weedle"
      , cryDiff = 33
      }
    , { indexNumber = 113
      , pokedexNumber = 14
      , name = "Kakuna"
      , cryDiff = 39
      }
    , { indexNumber = 114
      , pokedexNumber = 15
      , name = "Beedrill"
      , cryDiff = 41
      }
    , { indexNumber = 116
      , pokedexNumber = 85
      , name = "Dodrio"
      , cryDiff = 42
      }
    , { indexNumber = 117
      , pokedexNumber = 57
      , name = "Primeape"
      , cryDiff = 33
      }
    , { indexNumber = 118
      , pokedexNumber = 51
      , name = "Dugtrio"
      , cryDiff = 43
      }
    , { indexNumber = 119
      , pokedexNumber = 49
      , name = "Venomoth"
      , cryDiff = 32
      }
    , { indexNumber = 120
      , pokedexNumber = 87
      , name = "Dewgong"
      , cryDiff = 52
      }
    , { indexNumber = 123
      , pokedexNumber = 10
      , name = "Caterpie"
      , cryDiff = 20
      }
    , { indexNumber = 124
      , pokedexNumber = 11
      , name = "Metapod"
      , cryDiff = 51
      }
    , { indexNumber = 125
      , pokedexNumber = 12
      , name = "Butterfree"
      , cryDiff = 21
      }
    , { indexNumber = 126
      , pokedexNumber = 68
      , name = "Machamp"
      , cryDiff = 34
      }
    , { indexNumber = 128
      , pokedexNumber = 55
      , name = "Golduck"
      , cryDiff = 20
      }
    , { indexNumber = 129
      , pokedexNumber = 97
      , name = "Hypno"
      , cryDiff = 68
      }
    , { indexNumber = 130
      , pokedexNumber = 42
      , name = "Golbat"
      , cryDiff = 42
      }
    , { indexNumber = 131
      , pokedexNumber = 150
      , name = "Mewtwo"
      , cryDiff = 80
      }
    , { indexNumber = 132
      , pokedexNumber = 143
      , name = "Snorlax"
      , cryDiff = 0
      }
    , { indexNumber = 133
      , pokedexNumber = 129
      , name = "Magikarp"
      , cryDiff = 44
      }
    , { indexNumber = 136
      , pokedexNumber = 89
      , name = "Muk"
      , cryDiff = 34
      }
    , { indexNumber = 138
      , pokedexNumber = 99
      , name = "Kingler"
      , cryDiff = 59
      }
    , { indexNumber = 139
      , pokedexNumber = 91
      , name = "Cloyster"
      , cryDiff = 50
      }
    , { indexNumber = 141
      , pokedexNumber = 101
      , name = "Electrode"
      , cryDiff = 60
      }
    , { indexNumber = 142
      , pokedexNumber = 36
      , name = "Clefable"
      , cryDiff = 12
      }
    , { indexNumber = 143
      , pokedexNumber = 110
      , name = "Weezing"
      , cryDiff = 52
      }
    , { indexNumber = 144
      , pokedexNumber = 53
      , name = "Persian"
      , cryDiff = 47
      }
    , { indexNumber = 145
      , pokedexNumber = 105
      , name = "Marowak"
      , cryDiff = 31
      }
    , { indexNumber = 147
      , pokedexNumber = 93
      , name = "Haunter"
      , cryDiff = 53
      }
    , { indexNumber = 148
      , pokedexNumber = 63
      , name = "Abra"
      , cryDiff = 55
      }
    , { indexNumber = 149
      , pokedexNumber = 65
      , name = "Alakazam"
      , cryDiff = 82
      }
    , { indexNumber = 150
      , pokedexNumber = 17
      , name = "Pidgeotto"
      , cryDiff = 33
      }
    , { indexNumber = 151
      , pokedexNumber = 18
      , name = "Pidgeot"
      , cryDiff = 41
      }
    , { indexNumber = 152
      , pokedexNumber = 121
      , name = "Starmie"
      , cryDiff = 49
      }
    , { indexNumber = 153
      , pokedexNumber = 1
      , name = "Bulbasaur"
      , cryDiff = 33
      }
    , { indexNumber = 154
      , pokedexNumber = 3
      , name = "Venusaur"
      , cryDiff = 46
      }
    , { indexNumber = 155
      , pokedexNumber = 73
      , name = "Tentacruel"
      , cryDiff = 54
      }
    , { indexNumber = 157
      , pokedexNumber = 118
      , name = "Goldeen"
      , cryDiff = 20
      }
    , { indexNumber = 158
      , pokedexNumber = 119
      , name = "Seaking"
      , cryDiff = 43
      }
    , { indexNumber = 163
      , pokedexNumber = 77
      , name = "Ponyta"
      , cryDiff = 30
      }
    , { indexNumber = 164
      , pokedexNumber = 78
      , name = "Rapidash"
      , cryDiff = 41
      }
    , { indexNumber = 165
      , pokedexNumber = 19
      , name = "Rattata"
      , cryDiff = 8
      }
    , { indexNumber = 166
      , pokedexNumber = 20
      , name = "Raticate"
      , cryDiff = 21
      }
    , { indexNumber = 167
      , pokedexNumber = 33
      , name = "Nidorino"
      , cryDiff = 26
      }
    , { indexNumber = 168
      , pokedexNumber = 30
      , name = "Nidorina"
      , cryDiff = 24
      }
    , { indexNumber = 169
      , pokedexNumber = 74
      , name = "Geodude"
      , cryDiff = 54
      }
    , { indexNumber = 170
      , pokedexNumber = 77
      , name = "Porygon"
      , cryDiff = 53
      }
    , { indexNumber = 171
      , pokedexNumber = 142
      , name = "Aerodactyl"
      , cryDiff = 61
      }
    , { indexNumber = 173
      , pokedexNumber = 81
      , name = "Magnemite"
      , cryDiff = 53
      }
    , { indexNumber = 176
      , pokedexNumber = 4
      , name = "Charmander"
      , cryDiff = 32
      }
    , { indexNumber = 177
      , pokedexNumber = 7
      , name = "Squirtle"
      , cryDiff = 38
      }
    , { indexNumber = 178
      , pokedexNumber = 5
      , name = "Charmeleon"
      , cryDiff = 32
      }
    , { indexNumber = 179
      , pokedexNumber = 8
      , name = "Wartortle"
      , cryDiff = 40
      }
    , { indexNumber = 180
      , pokedexNumber = 6
      , name = "Charizard"
      , cryDiff = 41
      }
    , { indexNumber = 185
      , pokedexNumber = 43
      , name = "Oddish"
      , cryDiff = 27
      }
    , { indexNumber = 186
      , pokedexNumber = 44
      , name = "Gloom"
      , cryDiff = 29
      }
    , { indexNumber = 187
      , pokedexNumber = 45
      , name = "Vileplume"
      , cryDiff = 63
      }
    , { indexNumber = 188
      , pokedexNumber = 69
      , name = "Bellsprout"
      , cryDiff = 7
      }
    , { indexNumber = 189
      , pokedexNumber = 70
      , name = "Weepinbell"
      , cryDiff = 30
      }
    , { indexNumber = 190
      , pokedexNumber = 71
      , name = "Victreebel"
      , cryDiff = 46
      }
    ]
